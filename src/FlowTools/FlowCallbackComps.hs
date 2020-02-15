{-# LANGUAGE DataKinds #-}

module FlowTools.FlowCallbackComps
  ( waitForARIEventComp
  , waitForARIEventOnHandleComp
  , waitForChannelStateComp
  , waitForChannelTerminatedComp
  , waitForChannelTerminatedEvent
  , waitForChannelRingingOrConnectedComp
  , waitForDTMDReceivedComp
  , waitForAnyDTMDReceivedComp
  , waitForSomeDTMDReceivedComp
  , waitForCallProgressEvent
  , waitForCallConnectedEvent
  , waitForInitCallConnectedEvent
  , waitForChannelRinging
  , waitForChannelConnected
  , waitForInitChannelTerminated
  , playSoundComp
  , playMediaComp
  , playListComp
  , startRecording
  , ringChannelComp
  , playMOHChannelComp
  , withChannelComp
  , dialChannelComp
  , dialChannelConnectedComp
  , sendDTMFChannel
  , monitorOutboundCall
  , dialChannel
  , playSoundOnChannelComp
  , newRingChannelResource
  , newBridgeResource
  , bridgeChannelResource
  , muteChannelResource
  , newMOHChannelResource
  , PlaySoundCompleted (..)
  , RecordingCompleted (..)
  , ChannelTerminated (..)
  , CallProgressEvent (..)
  ) where

--import Prelude (String)
import Protolude
import Prelude (String)

import Utils
import ARICore
import ARIUtils

import FlowTools.FlowEvDistr
import FlowTools.FlowCallback
import FlowTools.Playlist

--type ARIEventCallbackFunc s = ARISrcEvent -> FlowContext s ()

loggerPath::String
loggerPath=getLoggerPath "FlowCallbackComp"

waitForARIEventComp :: HasARIEventDistributor s => ARIEventFilter -> FlowCallback s ARISrcEvent
waitForARIEventComp ariFilter =
  createCallbackCont (\cbFunc-> do
    monID <- addARIEventMonitor (EventMonitor ariFilter (\_ ev ->cbFunc ev >> return False))
    return $ removeARIEventMonitor monID)

waitForARIEventWithTransComp :: HasARIEventDistributor s => (ARISrcEvent -> Maybe ev') -> FlowCallback s ev'
waitForARIEventWithTransComp transEv =
  createCallbackCont (\cbFunc-> do
    monID <- addARIEventMonitorWithTrans transEv (\_ ev' -> cbFunc ev' >> return False)
    return $ removeARIEventMonitor monID)

waitForARIEventOnHandleComp :: (HasARIEventDistributor s , IsARIEventSource h) => h -> (ARIEvent -> Bool) -> FlowCallback s ARISrcEvent
waitForARIEventOnHandleComp h ariEvFilter = waitForARIEventComp (\(ARISrcEvent ariEv src) -> src==createSource h && ariEvFilter ariEv)

waitForARIEventOnHandleWithTransComp :: (HasARIEventDistributor s , IsARIEventSource h) => h -> (ARIEvent -> Maybe ev') -> FlowCallback s ev'
waitForARIEventOnHandleWithTransComp h transEv =
    waitForARIEventWithTransComp transEvWithHandle'
  where
    transEvWithHandle' (ARISrcEvent ariEv src)
      | src == createSource h = transEv ariEv
      | otherwise = Nothing

waitForChannelStateComp :: (HasARIEventDistributor s) => ChannelHandle -> ChannelState -> FlowCallback s ChannelState
waitForChannelStateComp chanHandle channelState = fmap (const channelState) (waitForARIEventOnHandleComp chanHandle (isChannelState [channelState]))

waitForDTMDReceivedComp :: (HasARIEventDistributor s) => ChannelHandle -> (DTMFDigit -> Bool) -> FlowCallback s DTMFDigit
waitForDTMDReceivedComp chanHandle dtmFIlter = waitForARIEventOnHandleWithTransComp chanHandle (digitReceivedFromEvent >=> (\d->if dtmFIlter d then return d else Nothing))

waitForAnyDTMDReceivedComp :: (HasARIEventDistributor s) => ChannelHandle -> FlowCallback s DTMFDigit
waitForAnyDTMDReceivedComp chanHandle = waitForDTMDReceivedComp chanHandle (const True)

waitForSomeDTMDReceivedComp :: (HasARIEventDistributor s, Foldable t) => ChannelHandle -> t DTMFDigit -> FlowCallback s DTMFDigit
waitForSomeDTMDReceivedComp chanHandle dtmfs = waitForDTMDReceivedComp chanHandle (`elem` dtmfs)

waitForChannelTerminatedComp :: (HasARIEventDistributor s) => ChannelHandle -> FlowCallback s ()
waitForChannelTerminatedComp chanHandle = void (waitForARIEventOnHandleComp chanHandle isFinalEvent)

waitForChannelTerminatedEvent :: (HasARIEventDistributor s) => ConnectedChannel -> FlowCallback s ChannelTerminated
waitForChannelTerminatedEvent (ChannelInState chanHandle) = const ChannelTerminated <$>  waitForChannelTerminatedComp chanHandle

waitForInitChannelTerminated :: (HasARIEventDistributor s) => InitiatedChannel -> FlowCallback s ChannelTerminated
waitForInitChannelTerminated (ChannelInState chanHandle) = const ChannelTerminated <$>  waitForChannelTerminatedComp chanHandle

waitForChannelRingingOrConnectedComp :: (HasARIEventDistributor s) => ChannelHandle -> FlowCallback s (OneOf' (FlowContextM s) RingingChannel ConnectedChannel)
waitForChannelRingingOrConnectedComp channelHandle = oneOfCall'
  (const (ChannelInState channelHandle) <$> waitForChannelStateComp channelHandle Ringing)
  (const (ChannelInState channelHandle) <$> waitForChannelStateComp channelHandle Up)

data ChannelTerminated = ChannelTerminated
withChannelComp :: (HasARIEventDistributor s) => ChannelHandle -> (ChannelHandle -> FlowCallback s a) ->FlowCallback s (OneOf' (FlowContextM s) a ChannelTerminated)
withChannelComp channelHandle chanFlowCallback = oneOfCall'
  (chanFlowCallback channelHandle)
  (const ChannelTerminated <$> waitForChannelTerminatedComp channelHandle)

data PlaySoundCompleted = PlaySoundCompleted
playSoundComp :: (HasARIEventDistributor s , PBCapabale hnd) =>  PBMedia -> hnd -> FlowCallback s PlaySoundCompleted
playSoundComp pbMedia hnd = do
  pbHndMaybe <- lift $ createHandleWithEvents (\eventHandler -> startPlaybackCmd pbMedia eventHandler hnd)
  case pbHndMaybe of
    Nothing -> throwError $ GenericError "Failed to create playback"
    Just pbHnd -> onCancel (const $ void $ unfail $ stopPlaybackCmd pbHnd) (fmap (const PlaySoundCompleted) (waitForARIEventOnHandleComp pbHnd isFinalEvent))

playSoundOnChannelComp :: (HasARIEventDistributor s) =>  PBMedia -> ConnectedChannel -> FlowCallback s PlaySoundCompleted
playSoundOnChannelComp pbMedia (ChannelInState chan) = playSoundComp pbMedia chan

ringChannelComp :: (HasARIEventDistributor s) =>  ConnectedChannel -> FlowCallback s Never
ringChannelComp chan = resourceCall $ newRingChannelResource chan

playMOHChannelComp :: (HasARIEventDistributor s) =>  ConnectedChannel -> FlowCallback s Never
playMOHChannelComp chan = resourceCall $ newMOHChannelResource chan

--sendDTMFChannelComp :: ConnectedChannel -> String -> FlowCallback s ()
--sendDTMFChannelComp chan dtmfStr =
--  lift $ sendDTMFChannel chan dtmfStr

dialChannelComp :: HasARIEventDistributor s => Tech -> Provider -> Party -> Party -> FlowCallback s (OneOf3' (FlowContextM s) RingingChannel ConnectedChannel ChannelTerminated)
dialChannelComp tech prov callerID destParty = do
  chanHandleMaybe <- lift $ createHandleWithEvents (dialOutCmd tech prov callerID destParty)
  case chanHandleMaybe of
    Nothing -> throwError $ GenericError "Failed to create channel"
    Just chanHandle -> compactOneOf3' <$> withChannelComp chanHandle waitForChannelRingingOrConnectedComp

dialChannelConnectedComp :: HasARIEventDistributor s => Tech -> Provider -> Party -> Party -> FlowCallback s (Maybe (ConnectedChannel, FlowCallback s ChannelTerminated))
dialChannelConnectedComp tech prov callerID destParty = do
  dialCallerRes <- dialChannelComp tech prov destParty callerID
  case dialCallerRes of
    Res3 ChannelTerminated -> liftIO $ infoM loggerPath "dialChannelConnectedComp : Call to Caller failed" >> return Nothing
    Res2 connectedCallerChannel callerDiscMonitor -> return $ Just  (connectedCallerChannel, callerDiscMonitor)
    Res1 _ringingCallerChannel callerConnectedDiscMonitor ->
      do waitForConnectedRes <- callerConnectedDiscMonitor
         case waitForConnectedRes of
           SecondCompleted' ChannelTerminated -> liftIO $ infoM loggerPath "dialChannelConnectedComp : Call to Caller failed after ringing" >> return Nothing
           FirstCompleted' connectedCallerChannel callerDiscMonitor -> return $ Just  (connectedCallerChannel, callerDiscMonitor)


monitorOutboundCall :: HasARIEventDistributor s => InitiatedChannel -> FlowCallback s (OneOf3' (FlowContextM s) RingingChannel ConnectedChannel ChannelTerminated)
monitorOutboundCall (ChannelInState chanHandle) = compactOneOf3' <$> withChannelComp chanHandle waitForChannelRingingOrConnectedComp

dialChannel :: HasARIEventDistributor s => Tech -> Provider -> Party -> Party -> FlowContext s InitiatedChannel
dialChannel tech prov callerID destParty = do
  chanHandleMaybe <- createHandleWithEvents (dialOutCmd tech prov callerID destParty)
  case chanHandleMaybe of
    Nothing -> throwError $ GenericError "Failed to dialChannel"
    Just chanHandle -> return $ ChannelInState chanHandle

data CallProgressEvent =  CallProgressRinging RingingChannel
                       |  CallProgressConnected ConnectedChannel
                       |  CallProgressTerminated ChannelTerminated

waitForCallProgressEvent :: HasARIEventDistributor s => InitiatedChannel -> FlowCallback s CallProgressEvent
waitForCallProgressEvent (ChannelInState chanHandle) = do
  raceRes <- raceCall (waitForChannelTerminatedComp chanHandle) (raceCall (waitForChannelStateComp chanHandle Up) (waitForChannelStateComp chanHandle Ringing))
  return $ case raceRes of
    Left _ -> CallProgressTerminated ChannelTerminated
    Right (Left _) -> CallProgressConnected $ ChannelInState chanHandle
    Right (Right _) -> CallProgressRinging $ ChannelInState chanHandle

waitForChannelConnected :: HasARIEventDistributor s => InitiatedChannel -> FlowCallback s  ConnectedChannel
waitForChannelConnected  (ChannelInState chanHandle) = do
  _ <- waitForChannelStateComp chanHandle Up
  let connectedChannel :: ConnectedChannel
      connectedChannel = ChannelInState chanHandle
  return connectedChannel

waitForChannelRinging :: HasARIEventDistributor s => InitiatedChannel -> FlowCallback s  RingingChannel
waitForChannelRinging  (ChannelInState chanHandle) = do
  _ <- waitForChannelStateComp chanHandle Ringing
  let ringingChannel :: RingingChannel
      ringingChannel = ChannelInState chanHandle
  return ringingChannel

waitForCallConnectedEventUnsafe :: HasARIEventDistributor s => ChannelHandle -> FlowCallback s (Either  ChannelTerminated ConnectedChannel)
waitForCallConnectedEventUnsafe chanHandle = do
  raceRes <- raceCall (waitForChannelTerminatedComp chanHandle) (waitForChannelStateComp chanHandle Up)
  return $ case raceRes of
    Left _ -> Left ChannelTerminated
    Right _ -> Right $ ChannelInState chanHandle

waitForCallConnectedEvent :: HasARIEventDistributor s => RingingChannel -> FlowCallback s (Either ChannelTerminated ConnectedChannel)
waitForCallConnectedEvent (ChannelInState chanHandle) = waitForCallConnectedEventUnsafe chanHandle

waitForInitCallConnectedEvent :: HasARIEventDistributor s => InitiatedChannel -> FlowCallback s (Either ChannelTerminated ConnectedChannel)
waitForInitCallConnectedEvent (ChannelInState chanHandle) = waitForCallConnectedEventUnsafe chanHandle

newBridgeResource :: ResourceC (FlowContextM s) BridgeHandle
newBridgeResource = newRes $ do
  hnd <- createBridgeCmd
  return (hnd, deleteBridgeCmd hnd)

bridgeChannelResource :: BridgeHandle -> ConnectedChannel -> ResourceC (FlowContextM s) ()
bridgeChannelResource bridgeHandle connectedChannel = newRes $ do
  addChannelToBridge bridgeHandle connectedChannel
  return ((), removeChannelFromBridge bridgeHandle connectedChannel)

muteChannelResource :: MuteDirection -> ConnectedChannel -> ResourceC (FlowContextM s) ()
muteChannelResource muteDir connectedChannel = newRes $ do
  startMuteChannel  muteDir connectedChannel
  return ((), stopMuteChannel muteDir connectedChannel)

newRingChannelResource :: ConnectedChannel -> ResourceC (FlowContextM s) ()
newRingChannelResource connectedChannel = newRes $ do
  void $ unfail $ startRingChannel connectedChannel
  return ((), void $ unfail $ stopRingChannel connectedChannel)


newMOHChannelResource :: ConnectedChannel -> ResourceC (FlowContextM s) ()
newMOHChannelResource connectedChannel = newRes $ do
  void $ unfail $ startMOHChannel connectedChannel
  return ((), void $ unfail $ stopMOHChannel connectedChannel)

playMediaComp ::  HasARIEventDistributor s => [PBMedia] -> ConnectedChannel -> FlowCallback s ()
playMediaComp mediaFiles connectedChannel =
  mapM_ (`playSoundOnChannelComp` connectedChannel) mediaFiles

playListComp ::  HasARIEventDistributor s => Playlist -> ConnectedChannel -> FlowCallback s ()
playListComp pbEntries = playMediaComp  (playlist pbEntries)-- connectedChannel

data RecordingCompleted = RecordingCompleted
startRecording :: (HasARIEventDistributor s , RecCapabale hnd) =>  RecordingParams -> hnd -> FlowCallback s (RecordingHandle, Text, FlowCallback s RecordingCompleted)
startRecording recParams hnd = do
  recHndAndIDMaybe <- lift $ createHandleWithEvents (\eventHandler -> startRecordingCmd recParams eventHandler hnd)
  case recHndAndIDMaybe of
    Nothing -> throwError $ GenericError "Failed to start recording"
    Just (recHnd, recID) -> return (recHnd, recID, fmap (const RecordingCompleted) (waitForARIEventOnHandleComp recHnd isFinalEvent))

--
{-
createIncomingFlowHandler :: HasARIEventDistributor s => HTTPEnv -> s -> FlowCallback s Bool -> NewChannelHandler  
createIncomingFlowHandler httpEnv s flowCallback 
    eventDispData -- EventDispData  
    chanEvData -- eventType and Channel details 
    channelHandle -- channelID
    evMonitor -- wrapped tchan
    = 
  do
    infoM loggerPath ("createIncomingFlowHandler entered")
    {-
    let sumFlow = do 
      enableHanldeMonitoring channelHandle
      runCall flowCallback (\_ -> exitFlow)
    fd <- createFlowData httpEnv eventDispData  
    void $ runFlow sumFlow fd s    
    -}
    infoM loggerPath ("createIncomingFlowHandler left")
-}

