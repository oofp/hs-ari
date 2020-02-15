{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module GroupCall.GroupCallSrv
( newGroupCallsSrv

, addEventMonitor
, removeEventMonitor

, getSnapshot
, dropCall
, dropAllCalls
, clearGroupCalls
, removeCallFromConference
, removeCallFromConferenceAndHold
, withCallChannelHandle
, holdCall
, unholdCall
, muteCall
, unmuteCall
, startRingCall
, stopRingCall
  {-
  GroupCallConfig (..),
  DialParams (..),
  ContactData (..),
  GroupCallsSrv,
  CallID (..),
  PlaybackID (..),
  RecordingID (..),
-}
) where

import Protolude hiding (handle)
import qualified Protolude as P
import Control.Lens
import Prelude (String)
import qualified Data.Map.Strict as M
import Data.Text
import ARICore hiding (bridgeID)
import Utils

import GroupCall.GroupCallData
import GroupCall.GroupCallAssets hiding (getSnapshot)
import qualified GroupCall.GroupCallAssets as A

loggerPath::String
loggerPath=getLoggerPath "GroupCalls"

type GroupCallEventDistributor s = EventDistributor GroupCallEvent (FlowContextM s) s
type GroupCallEvMonitor s = EventMonitor GroupCallEvent (FlowContextM s)

data GroupCallsState s=GroupCallsState
                                  { _srvName      :: Text
                                  , _gcAssets     :: GroupCallAssets
                                  , _config       :: GroupCallConfig
                                  , _eventDistr   :: GroupCallEventDistributor s
                                  }
makeLenses ''GroupCallsState

newGroupCallsSrv :: Text -> GroupCallConfig -> GroupCallsState s
newGroupCallsSrv serviceName cfg = GroupCallsState serviceName newGroupCallAssets  cfg newEventDistributor

--instance Show (GroupCallsState s) where
--  show gcs = unpack $ "GroupCallsState [" <> _name gcs <> "] calls:" <> P.show (M.size (_calls.gcs))

type GroupCallSrv s = Lens' s (GroupCallsState s)

addEventMonitor :: GroupCallSrv s -> GroupCallEvMonitor s -> FlowContext s EventID
addEventMonitor srv = addMonitor (srv.eventDistr) -- evMon

removeEventMonitor :: GroupCallSrv s ->  EventID -> FlowContext s ()
removeEventMonitor srv  = removeMonitor (srv.eventDistr) -- evID

notifyEventMonitor :: GroupCallSrv s ->  GroupCallEvent -> FlowContext s ()
notifyEventMonitor srv =  notifyEvent  (srv.eventDistr) -- ev

type LoggerFunc=String->String->IO ()
logSrv::GroupCallSrv s -> LoggerFunc-> Text -> FlowContext s ()
logSrv srv loggerFunc logStr = do
  nm <- use $ srv.srvName
  liftIO $ loggerFunc loggerPath (unpack (nm <> " " <> logStr))

gfa :: (GroupCallAssets -> a) -> GroupCallSrv s -> FlowContext s a
gfa getter srv  = getter <$> use (srv.gcAssets)

gca :: Lens' (GroupCallsState s) GroupCallAssets
gca = gcAssets

getSnapshot::GroupCallSrv s->FlowContext s ([GroupCall], [CallRecording], [PlayingState], [BridgeID])
getSnapshot srv = do
  logSrv srv infoM "getSnapshot"
  gfa A.getSnapshot srv

dropAllCalls :: GroupCallSrv s -> FlowContext s ()
dropAllCalls srv = do
  logSrv srv infoM "dropAllCalls"
  callIDs <- gfa A.getCallIDs srv
  P.forM_ callIDs (dropCall srv)

stopAllCallsSnooping :: GroupCallSrv s -> FlowContext s ()
stopAllCallsSnooping srv = do
  logSrv srv infoM "stopAllCallsSnooping"
  callIDs <- gfa A.getCallIDs srv
  P.forM_ callIDs (stopCallSnooping srv)

stopCallSnooping :: GroupCallSrv s -> CallID -> FlowContext s ()
stopCallSnooping srv callID = do
    logSrv srv infoM ("stopCallSnooping:" <> show callID)
    callMaybe <- gfa (A.getCallEntry callID) srv
    P.forM_ callMaybe dropSnoopCall
 where
  dropSnoopCall groupCallEntry =
    P.forM_ (_snoopingState groupCallEntry) (\snoopSt-> do
      void $ dropChannelCmd (_snoopChannel snoopSt)
      clearCallEntrySnoopData srv callID)

clearCallEntrySnoopData :: GroupCallSrv s -> CallID -> FlowContext s ()
clearCallEntrySnoopData srv callID = srv.gca %= A.updateCallEntry callID (snoopingState .~ Nothing)

--updateCallEntry :: GroupCallSrv s -> CallID -> (GroupCallEntry -> GroupCallEntry) -> FlowContext s ()
--updateCallEntry srv callID updateFunc = srv.calls %= M.adjust updateFunc callID

--updateGroupCall :: GroupCallSrv s -> CallID -> (GroupCall -> GroupCall) -> FlowContext s ()
--updateGroupCall srv callID updateFunc = updateCallEntry srv callID (gcCall %~ updateFunc)

dropCall::GroupCallSrv s -> CallID -> FlowContext s ()
dropCall srv callID  = do
    logSrv srv infoM ("dropCall:" <> show callID)
    callMaybe <- gfa (getCallEntry callID) srv
    P.forM_ callMaybe (dropChannelCmd . _handle)

clearGroupCalls::GroupCallSrv s -> FlowContext s ()
clearGroupCalls srv = do
  logSrv srv infoM "clearGroupCalls"
  dropAllCalls srv
  destroyAllBridges srv

destroyAllBridges :: GroupCallSrv s -> FlowContext s ()
destroyAllBridges srv = do
  srvBridges <- gfa A.getBridges srv
  void $ traverse (deleteBridgeCmd . _bridgeHandle) srvBridges
  srv.gca.bridges .= M.empty -- shortcut

destroyBridge ::GroupCallSrv s -> BridgeID -> FlowContext s ()
destroyBridge srv bridgeID  = do
  logSrv srv infoM ("destroyBridge:" <> show bridgeID)
  bridgeMaybe <- gfa (getBridge bridgeID) srv
  P.forM_ bridgeMaybe (\brd -> deleteBridgeCmd (_bridgeHandle brd) >> (srv.gca %= removeBridge (_brdgID brd)))

updateGroupCallAndNotify :: GroupCallSrv s -> CallID -> (GroupCall->GroupCall) -> FlowContext s ()
updateGroupCallAndNotify srv callID updateCall = do
  (srv.gca) %= A.updateGroupCall callID updateCall
  groupCallMaybe <- gfa (getCall callID) srv
  forM_ groupCallMaybe (\gc -> notifyEvent  (srv.eventDistr) (GroupCallChangeEvent gc))

removeCallFromConference :: GroupCallSrv s -> CallID  -> FlowContext s ()
removeCallFromConference srv callID = do
    gcCallEntryMaybe <- gfa (A.getCallEntry callID) srv
    forM_ gcCallEntryMaybe (\gcCallEntry -> forM_ (bridgeID (gcCallEntry ^. gcCall)) (\brgID ->
      do bridgeMaybe <- gfa (A.getBridge (BridgeID brgID)) srv
         forM_ bridgeMaybe (\brd -> void $ removeChannelFromBridgeCmd (brd ^.bridgeHandle) (gcCallEntry ^. handle))
         updateGroupCallAndNotify srv callID (\gc -> gc {bridgeID=Nothing})))

removeCallFromConferenceAndHold :: GroupCallSrv s -> CallID  -> FlowContext s ()
removeCallFromConferenceAndHold srv callID  = do
 removeCallFromConference srv callID
 holdCall srv callID

withCallChannelHandle :: GroupCallSrv s -> CallID -> (ChannelHandle -> FlowContext s a) -> FlowContext s ()
withCallChannelHandle srv callID cmd = do
  handleMaybe <- gfa (A.getCallChannelHandle callID) srv
  forM_ handleMaybe cmd

holdCall :: GroupCallSrv s -> CallID -> FlowContext s ()
holdCall srv callID = withCallChannelHandle srv callID startMOHChannelCmd >> updateGroupCallAndNotify srv callID (\gc -> gc {onHold=False})

unholdCall :: GroupCallSrv s -> CallID->FlowContext s ()
unholdCall srv callID = withCallChannelHandle srv callID stopMOHChannelCmd >> updateGroupCallAndNotify srv callID(\gc -> gc {onHold=True})

muteCall :: GroupCallSrv s->CallID->FlowContext s ()
muteCall srv callID  = withCallChannelHandle srv callID (startMuteChannelCmd MuteDirIn) >> updateGroupCallAndNotify srv callID(\gc -> gc {muted=True})

unmuteCall :: GroupCallSrv s->CallID->FlowContext s ()
unmuteCall srv callID  = withCallChannelHandle srv callID (stopMuteChannelCmd MuteDirIn) >> updateGroupCallAndNotify srv callID(\gc -> gc {muted=False})

startRingCall :: GroupCallSrv s->CallID->FlowContext s ()
startRingCall srv callID = withCallChannelHandle srv callID startRingingChannelCmd >> updateGroupCallAndNotify srv callID(\gc -> gc {ringing=True})

stopRingCall :: GroupCallSrv s->CallID->FlowContext s ()
stopRingCall srv callID = withCallChannelHandle srv callID stopRingingChannelCmd >> updateGroupCallAndNotify srv callID(\gc -> gc {ringing=False})

{-
getCalls::GroupCallsStateGetter s->FlowContext s CallsMap
getCalls getter = (calls.gcs) <$> getter

updateBridge1::GroupCallsStateUpdater s -> GroupCallsStateGetter s -> BridgeStaGroupCallSrv s teUpdater s
updateBridge1 updater getter b1 = do
  gcf <- getter
  updater $  gcf {gcs = (gcs gcf) {bridge1=b1} }

updateSnoopingBridge::GroupCallsStateUpdater s -> GroupCallsStateGetter s -> BridgeStateUpdater s
updateSnoopingBridge updater getter snoopBrd = do
  gcf <- getter
  updater $  gcf {gcs = (gcs gcf) {snoopBridge=snoopBrd} }

updateChannel::GroupCallsStateUpdater s -> GroupCallsStateGetter s ->Int->ChannelStateUpdater s
updateChannel updater getter index ch= do
--  (GroupCallsFlow name u g (GroupCallsState channels calls b1 cfg cb)) <- getter
--  updater $  GroupCallsFlow name u g (GroupCallsState (channels // [(index,ch)]) calls b1 cfg cb)
  gcf <- getter
  let curChannels=(channels.gcs) gcf
      newChannels = curChannels // [(index,ch)]
  updater $  gcf {gcs = (gcs gcf) {channels=newChannels} }

getChannel::GroupCallsStateGetter s ->Int->FlowContext s (ChannelFlow s)
getChannel getter index = ((V.! index).channels.gcs) <$> getter

getBridge1::GroupCallsStateGetter s -> FlowContext s (BridgeFlow s)
getBridge1 getter=  (bridge1 . gcs) <$> getter

getSnoopBridge::GroupCallsStateGetter s -> FlowContext s (BridgeFlow s)
getSnoopBridge getter=  (snoopBridge . gcs) <$> getter

getChannels::GroupCallsStateGetter s -> FlowContext s (ChannelsVector s)
getChannels getter= (channels . gcs) <$> getter

getCallIDs::GroupCallsStateGetter s -> FlowContext s [CallID]
getCallIDs getter = keys <$> getCalls getter

getCall::GroupCallsStateGetter s -> CallID -> FlowContext s (Maybe GroupCallEntry)
getCall getter callID = (M.lookup callID) <$> getCalls getter

getCallChannel::GroupCallsStateGetter s -> CallID -> FlowContext s (Maybe (ChannelFlow s))
getCallChannel getter callID = do
  callIndexMaybe <- getCall getter callID
  P.forM callIndexMaybe (\(_,index,_)->getChannel getter index)

removeCall::GroupCallsStateGetter s -> CallID -> FlowContext s ()
--removeCall (GroupCallsFlow name updater setter (GroupCallsState channels calls b1 cfg cb)) callID =
--  updater (GroupCallsFlow name updater setter (GroupCallsState channels (M.delete callID calls) b1 cfg cb))
removeCall getter callID = do
  gcf <-getter
  gcsUpdater gcf  $ gcf {gcs=(gcs gcf) {calls=M.delete callID ((calls . gcs) gcf)}}
--  updater (GroupCallsFlow name updater setter (GroupCallsState channels (M.delete callID calls) b1 cfg cb))

initChannelEntry::GroupCallsStateUpdater s -> GroupCallsStateGetter s->Int -> FlowContext s (ChannelFlow s)
initChannelEntry updater getter index =
    let channelName :: String
        channelName = "channel#"<>show index
    in initChannel channelName (updateChannel updater getter index) (getChannel getter index)

initChannelsVector::GroupCallsStateUpdater s -> GroupCallsStateGetter s->Int->FlowContext s (ChannelsVector s)
initChannelsVector updater getter maxChannels =
    sequenceA $ generate maxChannels (initChannelEntry updater getter)

initGroupCalls::String->GroupCallsStateUpdater s->GroupCallsStateGetter s->GroupCallConfig->GroupCallListener s->Lens' s (GroupCallsFlow s)->FlowContext s (GroupCallsFlow s)
initGroupCalls flowName updater getter cfg cb gcl =do
  channls <- initChannelsVector updater getter 32
  let callsMap = M.empty
  b1 <- initBridge "bridge1" (updateBridge1 updater getter)
  snpBrd <- initBridge "snoopBridge" (updateSnoopingBridge updater getter)

  return $ GroupCallsFlow flowName updater getter (GroupCallsState channls callsMap b1 snpBrd Nothing Nothing cfg cb initEventDistributor) gcl


notifyWithGroupCall :: GroupCallsStateGetter s->CallID->FlowContext s ()
notifyWithGroupCall getter callID =
  updateGroupCallEntry getter callID id -- fake update to trigger notification
-}

{-
createBridgeIfNeeded ::GroupCallsStateGetter s->FlowContext s ()
createBridgeIfNeeded getter = do
  b1 <- getBridge1 getter
  unless (bridgeExists b1) (void $ createBridge b1)

destroySnoopBridgeIfNeeded ::GroupCallsStateGetter s->FlowContext s ()
destroySnoopBridgeIfNeeded getter = do
  b1 <- getSnoopBridge getter
  when (bridgeExists b1) (destroyBridge b1)

createSnoopBridgeIfNeeded ::GroupCallsStateGetter s->FlowContext s ()
createSnoopBridgeIfNeeded getter = do
  b1 <- getSnoopBridge getter
  unless (bridgeExists b1) (void $ createBridge b1)

uninitGroupCalls::GroupCallsFlow s->FlowContext s ()
uninitGroupCalls gcf = do
  let getter = gcsGetter gcf
  logFlow getter infoM "uninitGroupCalls"
  dropAllCalls getter
  destroyBridgeIfNeeded getter

data DialParams= DialParams
  {soundToPlayOnConnect ::Maybe PBSoundParams
  ,addToBridge          ::Bool
  ,mute                 ::Bool
  ,hold                 ::Bool
  ,startWhisper         ::Bool
  }

hasDialParams::DialParams->Bool
hasDialParams (DialParams Nothing False False False False) = False
hasDialParams _ = True

data ContactData=ContactData
  {contactName::Text
  ,contactNumber::Text
  } deriving (Show)

findFreeChannelIndex :: GroupCallsStateGetter s-> FlowContext s (Maybe Int)
findFreeChannelIndex getter =
  (V.findIndex (not.channelAlive)).channels.gcs <$> getter

allocateCallAndChannel::GroupCallsStateGetter s->Text->ContactData->FlowContext s (Maybe (CallID,ChannelFlow s))
allocateCallAndChannel getter clnID cd = do
  (GroupCallsFlow name updater _getter (GroupCallsState channelsVec callsMap b1 b2 rec player cfg cb ed) ln) <- getter
  freeChannelIndex <- findFreeChannelIndex getter
  P.forM freeChannelIndex (\index->
    do
      logFlow getter debugM  ("allocateCallAndChannel:" <> " index picked:" <> show index)
      let channelFlow = channelsVec V.! index
      uuid <- liftIO  UUIDGen.nextRandom
      let callID = show uuid
          callIDVal = CallID callID
          groupCall =initGroupCall callID clnID (contactName cd) (contactNumber cd)
          newCalls=insert callIDVal (groupCall,index,Nothing) callsMap
      updater (GroupCallsFlow name updater getter (GroupCallsState channelsVec newCalls b1 b2 rec player cfg cb ed) ln)
      logFlow getter debugM ("allocateCallAndChannel:" <> "index picked:" <> show index <> " " <> show callIDVal)
      return (callIDVal, channelFlow))

updateGroupCallEntry :: GroupCallsStateGetter s->CallID->(GroupCallEntry->GroupCallEntry)->FlowContext s ()
updateGroupCallEntry getter callID updateCallEntry = do
  (GroupCallsFlow name updater setter (GroupCallsState channelsVec callsMap b1 b2 rec player cfg cb ed) ln) <- getter
  let newCallMap=adjust updateCallEntry callID callsMap
  updater (GroupCallsFlow name updater setter (GroupCallsState channelsVec newCallMap b1 b2 rec player cfg cb ed) ln)
  P.forM_ (lookup callID newCallMap) (\(newGroupCall,_,_)->notifyAll getter (GroupCallChangeEvent newGroupCall))

updateGroupCall::GroupCallsStateGetter s->CallID->(GroupCall->GroupCall)->FlowContext s ()
updateGroupCall getter callID func = do
  let updateCallEntry (groupCall,index,snoopData)=(func groupCall,index,snoopData)
  updateGroupCallEntry getter callID updateCallEntry

updateGroupCallStatus::GroupCallsStateGetter s->CallID->GroupCallStatus->FlowContext s ()
updateGroupCallStatus getter callID newStatus = do
  logFlow getter debugM ("updateGroupCallStatus:" <> " " <> show callID <> " state:" <> show newStatus)
  let updateGroupCallStatus' gcCall = gcCall {gcStatus=newStatus}
  updateGroupCall getter callID updateGroupCallStatus'

updateGroupCallBridge::GroupCallsStateGetter s->CallID->Maybe Text->ConferCallStatus->FlowContext s ()
updateGroupCallBridge getter callID bridgeIDMaybe confCallStatus=
  let updateGroupCallBridge' gcCall= gcCall {GroupCallData.bridgeID=bridgeIDMaybe, gcConfStatus=confCallStatus}
  in updateGroupCall getter callID updateGroupCallBridge'

updateGroupCallMute::GroupCallsStateGetter s->CallID->Bool->FlowContext s ()
updateGroupCallMute getter callID newMuted =
  let updateGroupCallMute' gcCall = gcCall {muted=newMuted}
  in updateGroupCall getter callID updateGroupCallMute'

updateGroupCallOnHold::GroupCallsStateGetter s->CallID->Bool->FlowContext s ()
updateGroupCallOnHold getter callID newOnHold =
  let updateGroupCallOnHold' gcCall=gcCall {onHold=newOnHold}
  in updateGroupCall getter callID updateGroupCallOnHold'

updateGroupCallRinging::GroupCallsStateGetter s->CallID->Bool->FlowContext s ()
updateGroupCallRinging getter callID newRinging =
  let updateCallField gcCall = gcCall {ringing=newRinging}
  in updateGroupCall getter callID updateCallField

updateGroupCallBridgeAndHold::GroupCallsStateGetter s->CallID->Maybe Text->ConferCallStatus->Bool->FlowContext s ()
updateGroupCallBridgeAndHold getter callID bridgeIDMaybe confStatus onH =
  let updateGroupCallBridgeAndHold' gcCall= gcCall {GroupCallData.bridgeID=bridgeIDMaybe, onHold=onH, gcConfStatus=confStatus}
  in updateGroupCall getter callID updateGroupCallBridgeAndHold'

updateGroupCallTalking::GroupCallsStateGetter s->CallID->Bool->FlowContext s ()
updateGroupCallTalking getter callID flTalking =
  let updateGroupCallTalking' groupCall =
        groupCall {talking=flTalking}
  in updateGroupCall getter callID updateGroupCallTalking'

updateGroupCallCPDetect::GroupCallsStateGetter s->CallID->CPDetectionStatus->FlowContext s ()
updateGroupCallCPDetect getter callID callProgressStatus = return ()
  -- let updateGroupCallCPStatus' groupCall =
  --      groupCall {cpStatus=callProgressStatus}
  -- in updateGroupCall getter callID updateGroupCallCPStatus'

updateGroupCallPlaying::GroupCallsStateGetter s->CallID->Bool->FlowContext s ()
updateGroupCallPlaying getter callID flPlaying =
  let updateGroupCallPlaying' groupCall =
        groupCall {playing=flPlaying}
  in updateGroupCall getter callID updateGroupCallPlaying'

makeCall::GroupCallsStateGetter s->DialParams->Text->ContactData->FlowContext s (Maybe CallID)
makeCall getter params clnID cd@(ContactData _ dest) = do
  logFlow getter infoM (" makeCall:" <> show cd)
  cfg <- (config.gcs) <$> getter
  callChannelMaybe <- allocateCallAndChannel getter clnID cd
  -- join is needed to collapse two Maybes inside ret value
  join <$> P.forM callChannelMaybe (\(callID, channel)->
    handleBoth_
      (dialOut SIP (Party dest) (Provider (provider cfg)) (Party (callerID cfg))
        (onCallProgress getter callID params)
        (onCallEnded getter callID)
        channel)
      (do logFlow getter infoM "makeCall: dialOut failed"
          removeCall getter callID
          return Nothing)  -- failure to dialOut
      (do logFlow getter infoM "makeCall: dialOut completed"
          updateGroupCallStatus getter callID Dialing
          return (Just callID)))           -- dialingOut ; return CallID

onCallProgress::GroupCallsStateGetter s->CallID->DialParams->ChannelCallState->FlowContext s Bool
onCallProgress getter callID _dialParams Ringback  =   updateGroupCallStatus getter callID Ringing >> continue
onCallProgress getter callID dialParams ChannelFlow.Connected =  do
  logFlow getter infoM ("onCallProgress(Connected): " <> show callID <> " handling started")
  updateGroupCallStatus getter callID GroupCallData.Connected
  when (hasDialParams dialParams) (handleDialParams getter callID dialParams)
  logFlow getter infoM ("onCallProgress(Connected): " <> show callID <> " handling completed")
  continue
onCallProgress getter callID _dialParams channelState = do
  logFlow getter infoM ("onCallProgress: " <> show callID <> " unsupported channel state:" <> show channelState)
  continue


onCallEnded::GroupCallsStateGetter s->CallID->FlowContext s Bool
onCallEnded getter callID= do
  logFlow getter infoM ("onCallEnded: " <> show callID)
  updateGroupCallStatus getter callID Disconnected
  removeCall getter callID
  logFlow getter infoM ("onCallEnded: " <> show callID <> " handling completed")
  continue

handleDialParams::GroupCallsStateGetter s->CallID->DialParams->FlowContext s ()
handleDialParams getter callID dialParams =
    case soundToPlayOnConnect dialParams of
      Nothing -> handleDialParams'
      (Just theSound) ->  playSoundCall getter callID (PBSound theSound) handleDialParams'
  where
    handleDialParams' = do
        when (addToBridge dialParams) (addCallToConference getter callID)
        when (mute dialParams) (muteCall getter callID)
        when (hold dialParams) (holdCall getter callID)
        when (startWhisper dialParams) (startCallWhispering getter callID)
        withCallChannel getter callID (enableChannelTalkDetect handleTalkDetect handleCPDetect)
    handleTalkDetect flTalk = updateGroupCallTalking getter callID flTalk >> return True
    handleCPDetect cpDetect = updateGroupCallCPDetect getter callID (cpStatus cpDetect) >>
                              logFlow getter infoM ("handleCPDetect: " <> show callID <> " detResult:" <> show cpDetect) >>
                              return True

withCallChannel::GroupCallsStateGetter s->CallID->(ChannelFlow s->FlowContext s a)->FlowContext s ()
withCallChannel getter callID op= do
  channelMaybe <- getCallChannel getter callID
  P.forM_ channelMaybe op

withCallChannelHandle::GroupCallsStateGetter s->CallID->(ChannelHandle->FlowContext s a)->FlowContext s ()
withCallChannelHandle getter callID func= withCallChannel getter callID (getChannelHandle >=> func)

addCallToConference :: GroupCallsStateGetter s->CallID->FlowContext s ()
addCallToConference getter callID  = do
    createBridgeIfNeeded getter
    addCallToConference' getter callID getBridge1 Conferenced

addCallToConference' :: GroupCallsStateGetter s -> CallID -> (GroupCallsStateGetter s ->FlowContext s (BridgeFlow s)) ->ConferCallStatus-> FlowContext s ()
addCallToConference' getter callID  bridgeGetter confCallStatus = do
    bridgeHandleFlow <- getBridgeHandle <$> bridgeGetter getter
    bridgeHandle <- bridgeHandleFlow
    withCallChannel getter callID (addCallToBridge bridgeHandle)
    updateGroupCallBridge getter callID (Just (getID bridgeHandle)) confCallStatus
  where addCallToBridge bridgeHandle channelFlow = void $ getChannelHandle channelFlow >>= addChannelToBridgeFlow bridgeHandle

retrieveAndAddCallToConference :: GroupCallsStateGetter s->CallID->FlowContext s ()
retrieveAndAddCallToConference getter callID  = do
  createBridgeIfNeeded getter
  retrieveAndAddCallToConference' getter callID getBridge1 Conferenced

retrieveAndAddCallToConference' :: GroupCallsStateGetter s->CallID->(GroupCallsStateGetter s ->FlowContext s (BridgeFlow s)) -> ConferCallStatus -> FlowContext s ()
retrieveAndAddCallToConference' getter callID bridgeGetter confCallStatus = do
    bridgeHandleFlow <- getBridgeHandle <$> bridgeGetter getter
    bridgeHandle <- bridgeHandleFlow
    withCallChannelHandle getter callID stopMOHChannelFlow
    withCallChannel getter callID (addCallToBridge bridgeHandle)
    updateGroupCallBridgeAndHold getter callID (Just (getID bridgeHandle)) confCallStatus False
  where addCallToBridge bridgeHandle channelFlow = void $ getChannelHandle channelFlow >>= addChannelToBridgeFlow bridgeHandle

removeCallFromConference :: GroupCallsStateGetter s->CallID->FlowContext s ()
removeCallFromConference getter callID  = removeCallFromConference' getter callID getBridge1

removeCallFromConfBridge :: GroupCallsStateGetter s->CallID->(GroupCallsStateGetter s ->FlowContext s (BridgeFlow s)) -> FlowContext s ()
removeCallFromConfBridge getter callID  bridgeGetter = do
    bridgeHandleFlow <- getBridgeHandle <$> bridgeGetter getter
    bridgeHandle <- bridgeHandleFlow
    withCallChannel getter callID (removeCallFromBridge bridgeHandle)
  where removeCallFromBridge bridgeHandle channelFlow = void $ getChannelHandle channelFlow >>= removeChannelFromBridgeFlow bridgeHandle

stopCallSnooping :: GroupCallsStateGetter s->CallID->FlowContext s ()
stopCallSnooping getter callID = do
    logFlow getter infoM ("stopCallSnooping:" <> show callID)
    callMaybe <- getCall getter callID
    P.forM_ callMaybe dropSnoopCall
    -- removeCall getter callID let call to be disconnected by event
    -- !! clear snopData and snopBridgeID at entry
 where
  dropSnoopCall (_groupCall,_index, snoopDataMaybe) =
    P.forM_ snoopDataMaybe (\snopData-> do
      ch <- getChannel getter (snoopChannelIndex snopData)
      dropChannel ch
      clearCallEntrySnoopData getter callID)


clearCallEntrySnoopData :: GroupCallsStateGetter s->CallID->FlowContext s ()
clearCallEntrySnoopData getter callID = updateGroupCallEntry getter callID clearCallEntrySnoopData'
  where
    clearCallEntrySnoopData' :: GroupCallEntry -> GroupCallEntry
    clearCallEntrySnoopData' (groupCall, chanIndex, _)=
      (groupCall {snoopBridgeID=Nothing}, chanIndex, Nothing)

callExist :: GroupCallsStateGetter s->CallID->FlowContext s Bool
callExist getter callID =
  (member callID).calls.gcs <$> getter

startCallSnooping :: GroupCallsStateGetter s->CallID->FlowContext s ()
startCallSnooping getter callID = do
    logFlow getter infoM ("startCallSnooping:" <> show callID)
    flCallExist <- callExist getter callID
    when flCallExist (startCallSnooping' getter callID)

startCallSnooping' :: GroupCallsStateGetter s->CallID-> FlowContext s ()
startCallSnooping' getter callID = do
  logFlow getter infoM ("startCallSnooping':" <> show callID <> "call found")
  stopAllCallsSnooping getter
  withCallChannel getter callID (\channelFlow -> do
    freeChannelIndexMaybe <- findFreeChannelIndex getter
    P.forM_ freeChannelIndexMaybe (\channelIndex -> do
      snoopCh <- getChannel getter channelIndex
      snoopChannel SpyBoth WhisperOut snoopCh
        (\state-> logFlow getter infoM  ("onCallProgress at snoop channel:" <> show state <> " index:" <> show channelIndex) >> continue)
        (logFlow getter infoM ("onCallEnded at snoop channel index:" <> show channelIndex)  >> continue)
        channelFlow
      snpBridgeID <-addChannelToSnoopingBridge getter channelIndex
      updateGroupCallEntry getter callID
        (\(groupCall, index, _) -> (groupCall {snoopBridgeID=Just snpBridgeID} , index , Just (SnoopingState SpyBoth WhisperOut channelIndex)))
      ))

addChannelToSnoopingBridge :: GroupCallsStateGetter s->Int->FlowContext s Text
addChannelToSnoopingBridge getter channelIndex = do
    channelFlow <- getChannel getter channelIndex
    createSnoopBridgeIfNeeded getter
    bridgeHandleFlow <- getBridgeHandle <$> getSnoopBridge getter
    bridgeHandle <- bridgeHandleFlow
    void $ getChannelHandle channelFlow >>= addChannelToBridgeFlow bridgeHandle
    return $ getID bridgeHandle

startCallWhispering :: GroupCallsStateGetter s->CallID->FlowContext s ()
startCallWhispering getter callID = do
    logFlow getter infoM ("startCallWhispering:" <> show callID)
    createSnoopBridgeIfNeeded getter
    retrieveAndAddCallToConference' getter callID getSnoopBridge Whispering

stopCallWhispering :: GroupCallsStateGetter s->CallID->FlowContext s ()
stopCallWhispering getter callID = do
    logFlow getter infoM ("stopCallWhispering:" <> show callID)
    removeCallFromConferenceAndHold' getter callID getSnoopBridge

continue :: FlowContext s Bool
continue = return True

playSound::PBMedia->FlowContext s ()->ChannelFlow s->FlowContext s ()
playSound mediaFile playCompletedHandler channelFlow = do
    channelHandle <- getChannelHandle channelFlow
    void $ createPlaybackFlow channelHandle mediaFile playEventHandler
  where
    playEventHandler PlaybackFinished _ = playCompletedHandler>>continue
    playEventHandler _ _ = continue

playSoundCall::GroupCallsStateGetter s->CallID->PBMedia->FlowContext s ()->FlowContext s ()
playSoundCall getter callID mediaFile playCompletedHandler = withCallChannel getter callID
    (\chanFlow -> playSound mediaFile (setPlaying False >> logFlow getter infoM ("playCompleted: " <> show callID) >> playCompletedHandler) chanFlow >> setPlaying True)
  where
    setPlaying = updateGroupCallPlaying getter callID

dialParamsFromDialMode :: DialMode -> DialParams
dialParamsFromDialMode DialModeNothing      = DialParams Nothing False False False False
dialParamsFromDialMode DialModeJoinPlain    = DialParams Nothing True False False False
dialParamsFromDialMode DialModeJoin         = DialParams (Just $ PBSoundParams "conf-placeintoconf") True False False False
-- dialParamsFromDialMode DialModeJoin     = DialParams (Just $ PBSoundParams "beep") True False False False
dialParamsFromDialMode DialModeBargeIn      = DialParams (Just $ PBSoundParams "confbridge-muted") True True False False
dialParamsFromDialMode DialModeHold         = DialParams (Just $ PBSoundParams "conf-waitforleader") False False True False
dialParamsFromDialMode DialModeWhisper      = DialParams (Just $ PBSoundParams "conf-placeintoconf") False False False True

startRecPlaying  :: GroupCallsStateGetter s -> Text -> Text -> FlowContext s ()
startRecPlaying  getter clnPlayID recordingID = do
  stopRecPlayingIfNeeded getter
  createBridgeIfNeeded getter
  bridgeHandleFlow <- getBridge1 getter
  bridgeHandle <- getBridgeHandle bridgeHandleFlow
  pbHandle <- createPlaybackFlow bridgeHandle (PBRecording (PBRecordingParams recordingID)) (handleRecPlaybackEvent getter)
  setRecPlaying getter (Just (StateAndHandle (PlayingState (getID pbHandle) clnPlayID PlayInitiated False) pbHandle))
  sendRecPlayingEvent getter

setRecPlaying :: GroupCallsStateGetter s -> Maybe PlayerRecSH -> FlowContext s ()
setRecPlaying getter recPlayerMaybe = do
  gcf <- getter
  gcsUpdater gcf (gcf { gcs = (gcs gcf) { playerSH = recPlayerMaybe } } )
  sendRecPlayingEvent getter

stopRecPlayingIfNeeded :: GroupCallsStateGetter s -> FlowContext s ()
stopRecPlayingIfNeeded getter = do
  pbMaybe <- (playerSH.gcs) <$> getter
  case pbMaybe of
    (Just sh) -> stopRecPlaying getter (playID (getState sh))
    Nothing -> return ()

forRecPlayer :: GroupCallsStateGetter s -> (PlayerRecSH -> FlowContext s a) -> FlowContext s ()
forRecPlayer getter f = do
  gcState <- gcs <$> getter
  P.forM_ (playerSH gcState) f

forRecPlayerIfMatch :: GroupCallsStateGetter s -> Text -> (PlayerRecSH -> FlowContext s a) -> FlowContext s ()
forRecPlayerIfMatch getter pbID f =
  forRecPlayer getter (\sh->when (playID (getState sh)==pbID) (void (f sh)))

sendRecPlayingEvent :: GroupCallsStateGetter s -> FlowContext s ()
sendRecPlayingEvent getter =
    -- forRecPlayer getter (globalCallListener (callback gcs) . PlayingStateChangeEvent . getState)
    forRecPlayer getter  sendRecPlayingEvent'
  where
    sendRecPlayingEvent' sh = notifyAll getter (PlayingStateChangeEvent (getState sh))

stopRecPlaying  :: GroupCallsStateGetter s -> Text -> FlowContext s ()
stopRecPlaying getter playingID =
  forRecPlayerIfMatch getter playingID (deletePlaybackFlow.getHandle )

handleRecPlaybackEvent:: GroupCallsStateGetter s ->PlaybackEventType->  PlaybackHandle -> FlowHandler s
handleRecPlaybackEvent getter pbEvType pbHandle=
    forRecPlayerIfMatch getter (getID pbHandle)  updateRecPlayerStatus >> continue
  where
    updateRecPlayerStatus _ = do
      gcf <- getter
      gcsUpdater gcf (gcf {gcs= (gcs gcf) { playerSH = fmap (\sh->sh {getState= (getState sh) {playStatus=playStatusFromEvent pbEvType} }) (playerSH (gcs gcf))} })
      --gcsUpdater gcf (gcf {gcs= (gcs gcf) { playerSH = fmap (\sh->sh {getState= (getState sh) {playPaused=True} }) (playerSH (gcs gcf)) } })
      sendRecPlayingEvent getter
      when (pbEvType==PlaybackFinished) (setRecPlaying getter Nothing)
    playStatusFromEvent PlaybackFinished = PlayFinished
    playStatusFromEvent PlaybackStarted  = PlayStarted

controlRecPlaying :: GroupCallsStateGetter s -> Text -> PBControlOp -> FlowContext s ()
controlRecPlaying getter plbID op =
    forRecPlayerIfMatch getter plbID controlRecPlaying'
  where
    controlRecPlaying' ch = do
       void $ controlPlaybackFlow (getHandle ch) op
       when (op==PBCtrlPause || op==PBCtrlUnpause) (updatePauseFlag (op==PBCtrlPause))
    updatePauseFlag flag = do
       gcf <- getter
       gcsUpdater gcf (gcf {gcs= (gcs gcf) { playerSH = fmap (\sh->sh {getState= (getState sh) {playPaused=flag} }) (playerSH (gcs gcf)) } })
       sendRecPlayingEvent getter

recordingParams::RecordingParams
recordingParams = RecordingParams RecFormatWav 1200 10 True RecTermStar

startRecording :: GroupCallsStateGetter s -> Text -> FlowContext s ()
startRecording getter rcClientReqID = do
  stopRecorderIfNeeded getter
  createBridgeIfNeeded getter
  bridgeHandleFlow <- getBridge1 getter
  bridgeHandle <- getBridgeHandle bridgeHandleFlow
  recHandle <- createRecordingFlow bridgeHandle recordingParams (handleRecorderEvent getter)
  setRecorder getter (Just (StateAndHandle (CallRecording (getID recHandle) rcClientReqID RecordingInitiated False False False) recHandle))
  sendRecordingEvent getter

setRecorder :: GroupCallsStateGetter s -> Maybe RecorderSH -> FlowContext s ()
setRecorder getter recorderMaybe = do
  gcf <- getter
  gcsUpdater gcf (gcf { gcs = (gcs gcf) { recorderSH = recorderMaybe } } )
  sendRecordingEvent getter

stopRecorderIfNeeded :: GroupCallsStateGetter s -> FlowContext s ()
stopRecorderIfNeeded getter = do
  recMaybe <- (recorderSH.gcs) <$> getter
  case recMaybe of
    (Just sh) -> stopRecording getter (recID (getState sh)) False
    Nothing -> return ()

forRecorder :: GroupCallsStateGetter s -> (RecorderSH -> FlowContext s a) -> FlowContext s ()
forRecorder getter f = do
  gcState <- gcs <$> getter
  P.forM_ (recorderSH gcState) f

forRecorderIfMatch :: GroupCallsStateGetter s -> Text -> (RecorderSH -> FlowContext s a) -> FlowContext s ()
forRecorderIfMatch getter rcID f =
  forRecorder getter (\sh->when (recID (getState sh)==rcID) (void (f sh)))

sendRecordingEvent :: GroupCallsStateGetter s -> FlowContext s ()
sendRecordingEvent getter =
    forRecorder getter  sendRecordingEvent'
  where
   sendRecordingEvent' sh = notifyAll getter (BridgeRecordingChangeEvent (getState sh))

stopRecording  :: GroupCallsStateGetter s -> Text -> Bool -> FlowContext s ()
stopRecording getter rcID flStore=
    forRecorderIfMatch getter rcID (stopRecording' flStore . getHandle)
  where
    stopRecording' True h =  stopStoreRecordingFlow h
    stopRecording' False h = stopDiscardRecordingFlow h

handleRecorderEvent:: GroupCallsStateGetter s -> RecordingEventType -> RecordingHandle -> FlowHandler s
handleRecorderEvent getter recEvType rcHnd=
    forRecorderIfMatch getter (getID rcHnd)  updateRecorderStatus >> continue
  where
  updateRecorderStatus _ = do
    gcf <- getter
    gcsUpdater gcf (gcf {gcs= (gcs gcf) { recorderSH = fmap (\sh->sh {getState= (getState sh) {recStatus=recStatusFromEvent recEvType} }) (recorderSH (gcs gcf))} })
    sendRecordingEvent getter
    when (recEvType==AIRD.RecordingFinished || recEvType==AIRD.RecordingFailed) (setRecorder getter Nothing)

  recStatusFromEvent AIRD.RecordingFinished  = GroupCallData.RecordingFinished
  recStatusFromEvent AIRD.RecordingFailed    = GroupCallData.RecordingFailed
  recStatusFromEvent AIRD.RecordingStarted   = GroupCallData.RecordingStarted


controlRecording :: GroupCallsStateGetter s -> Text -> (RecordingHandle->FlowContext s HTTPOK)->(CallRecording->CallRecording)->FlowContext s ()
controlRecording getter rcID recOp stateFunc  =
    forRecorderIfMatch getter rcID controlRecording'
  where
    controlRecording' ch = do
       void $ recOp (getHandle ch)
       gcf <- getter
       gcsUpdater gcf (gcf {gcs= (gcs gcf) { recorderSH = fmap (\sh->sh {getState= stateFunc (getState sh) }) (recorderSH (gcs gcf)) } })
       sendRecordingEvent getter


muteRecording :: GroupCallsStateGetter s -> Text -> FlowContext s ()
muteRecording getter rcID  = controlRecording getter rcID muteRecordingFlow (setRecordingMute True)

unmuteRecording :: GroupCallsStateGetter s -> Text -> FlowContext s ()
unmuteRecording getter rcID  = controlRecording getter rcID unmuteRecordingFlow (setRecordingMute False)

pauseRecording :: GroupCallsStateGetter s -> Text -> FlowContext s ()
pauseRecording getter rcID  = controlRecording getter rcID pauseRecordingFlow (setRecordingPaused True)

resumeRecording :: GroupCallsStateGetter s -> Text -> FlowContext s ()
resumeRecording getter rcID  = controlRecording getter rcID resumeRecordingFlow (setRecordingPaused False)

setRecordingMute :: Bool -> CallRecording->CallRecording
setRecordingMute flMute rcState = rcState {recMuted=flMute}

setRecordingPaused :: Bool -> CallRecording->CallRecording
setRecordingPaused flPaused rcState = rcState {recPaused=flPaused}

handleGroupCallRequest :: GroupCallReq -> GroupCallsFlow s -> FlowContext s ()
handleGroupCallRequest (DialReq dialReqParams) gcf = void $ handleMakeCallRequest dialReqParams gcf
handleGroupCallRequest (AddToConf (AddToConfReqParams callID)) gcf = addCallToConference (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (AddToConf2 (AddToConfReqParams2 callID)) gcf = retrieveAndAddCallToConference (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (RemoveFromConf (RemoveFromConfReqParams callID)) gcf = removeCallFromConference (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (RemoveFromConf2 (RemoveFromConfReqParams2 callID)) gcf = removeCallFromConferenceAndHold (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (HoldCall (HoldCallReqParams callID)) gcf = holdCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (UnholdCall (UnholdCallReqParams callID)) gcf = unholdCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (DropCall (DropCallReqParams callID)) gcf = dropCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (MuteCall (MuteCallReqParams callID)) gcf = muteCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (UnmuteCall (UnmuteCallReqParams callID)) gcf = unmuteCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (StartRing (StartRingCallReqParams callID)) gcf = startRingCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (StopRing (StopRingCallReqParams callID)) gcf = stopRingCall (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (PlayCall (PlayCallReqParams callID soundFile)) gcf = playSoundCall (gcsGetter gcf) (CallID callID) (PBSound (PBSoundParams soundFile)) (return ())
handleGroupCallRequest DropAll gcf = logFlow (gcsGetter gcf) infoM "dropAll received" >> dropAllCalls (gcsGetter gcf)
handleGroupCallRequest (StartSnoopCall (StartSnoopingReqParams callID)) gcf = startCallSnooping (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (StopSnoopCall (StopSnoopingReqParams callID)) gcf = stopCallSnooping (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (StartWhisperingCall (StartWhisperingReqParams callID)) gcf = startCallWhispering (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (StopWhisperingCall (StopWhisperingReqParams callID)) gcf = stopCallWhispering (gcsGetter gcf) (CallID callID)
handleGroupCallRequest (StartRecPlaying  (StartRecPlayingReqParams clnPlayID recordName)) gcf = startRecPlaying  (gcsGetter gcf)  clnPlayID recordName
handleGroupCallRequest (StopRecPlaying  (StopRecPlayingReqParams plbID)) gcf = stopRecPlaying  (gcsGetter gcf)  plbID
handleGroupCallRequest (ControlRecPlaying  (ControlRecPlayingReqParams plbID ctrlOp)) gcf = controlRecPlaying  (gcsGetter gcf)  plbID ctrlOp
handleGroupCallRequest (StartRecording  (StartRecordingReqParams rcClientReqID)) gcf = startRecording (gcsGetter gcf) rcClientReqID
handleGroupCallRequest (StopRecording  (StopRecordingReqParams rcID flStore)) gcf = stopRecording (gcsGetter gcf) rcID flStore
handleGroupCallRequest (MuteRecording  (MuteRecordingReqParams rcID)) gcf = muteRecording (gcsGetter gcf) rcID
handleGroupCallRequest (UnmuteRecording  (UnmuteRecordingReqParams rcID)) gcf = unmuteRecording (gcsGetter gcf) rcID
handleGroupCallRequest (PauseRecording  (PauseRecordingReqParams rcID)) gcf = pauseRecording (gcsGetter gcf) rcID
handleGroupCallRequest (ResumeRecording  (ResumeRecordingReqParams rcID)) gcf = resumeRecording (gcsGetter gcf) rcID
handleGroupCallRequest GetSnapshot gcf = getSnapshot (gcsGetter gcf)
handleGroupCallRequest (CallbackRequest _) gcf = logFlow (gcsGetter gcf) infoM "CallbackRequest should not come here"
handleGroupCallRequest (CallbackParRequest _) gcf = logFlow (gcsGetter gcf) infoM "CallbackParRequest should not come here"


handleMakeCallRequest :: DialReqParams -> GroupCallsFlow s -> FlowContext s (Maybe CallID)
handleMakeCallRequest (DialReqParams clnID numberToDial nameToDial dialingMode) gcf = makeCall (gcsGetter gcf) (dialParamsFromDialMode dialingMode) clnID (ContactData nameToDial numberToDial)
-}

