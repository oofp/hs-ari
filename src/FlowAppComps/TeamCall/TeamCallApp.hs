{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FlowAppComps.TeamCall.TeamCallApp
  ( TeamRequest
  , TeamEvent
  , AppState
  , HasAppState (..)
  , teamCallApp
  ) where

import Protolude
import Prelude (String)
-- import Data.Aeson hiding ((.=), defaultOptions)
import Control.Lens
import Data.Text
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Data.Set as Set
import Data.List as L

import FlowAppComps.DialingPlan
import FlowAppComps.FlowAppHelpers

import Utils
import ARICore
import ARIUtils
import FlowTools

import FlowAppComps.TeamCall.TeamCallData

loggerPath::String
loggerPath=getLoggerPath "TeamCallApp"

type MemeberID = Text

data JoinBridgeParams = JoinBridgeParams BridgeHandle
data ExitHandlingParams = ExitHandlingParams MemeberID

data InternalRequest = JoinBridge JoinBridgeParams
                     | ExitHandling ExitHandlingParams

data AppState s = AppState
                     { _members :: MembersMap
                     , _talking :: TalkingMembersSet
                     , _meetingState :: MeetingState
                     , _rsEventDistr :: EventDistributor TeamEvent (FlowContextM s) s
                     , _reqDistr :: EventDistributor TeamRequest (FlowContextM s) s
                     , _internReqDistr :: EventDistributor InternalRequest (FlowContextM s) s
                     , _lastMemberID :: Int
                     }
makeLenses ''AppState

instance EmptyData (AppState s) where
  emptyData = AppState M.empty Set.empty MeetingStateNull newEventDistributor newEventDistributor newEventDistributor 0

--type HasAppState s = SubSvc (AppState s) s
class HasAppState s where
  subSvc :: Lens' s (AppState s)

instance FlowSubSvc TeamEvent TeamRequest (AppState s) s where
  reqDisp = reqDistr
  evDistr = rsEventDistr

--instance GetSnapshot (AppState s) BasicCallRS  where
--  snapshot = _stateRS

debugLog :: (MonadIO m) => Text -> m ()
debugLog txt = liftIO $ debugM loggerPath (unpack txt)

createIVRMenu :: [Text] -> DTMFBuffer -> IVRCallbackCont s (Maybe DTMFDigit)
createIVRMenu promptFiles dtmfBuf=
  ivrMenu IVRMenuParams
    { _promptPlaybacks = (PlayEntry . FilePB) <$> promptFiles
    , _wrongKeyPlaybacks = [PlayEntry $ FilePB "vm-pls-try-again"]
    , _timeoutPlaybacks = [PlayEntry $ FilePB "vm-sorry"]
    , _validKeys = dtmfBuf
    , _repeatKey = Nothing
    , _maxAttempts = 4
    , _timeout = 5000000
    }

sendInternalReq :: (HasAppState s) => InternalRequest -> FlowContext s ()
sendInternalReq req = notifyEvent (subSvc.internReqDistr) req

waitForMembersEvent :: (HasAppState s) => FlowCallback s Members
waitForMembersEvent = monitorCallbackWithTrans (subSvc.rsEventDistr) getMembersEvent
  where
    getMembersEvent (MembersEvent mEvent) = Just mEvent
    getMembersEvent _ = Nothing

requestComp :: (HasAppState s) => (TeamRequest-> Maybe rq) -> FlowCallback s rq
requestComp = monitorCallbackWithTrans (subSvc.reqDistr)  -- transReq

waitForInitMeeting :: HasAppState s => FlowCallback s InitMeetingData
waitForInitMeeting = requestComp isEvent
  where
    isEvent (InitMeeting initMeetingData) = Just initMeetingData
    isEvent _ = Nothing

data StopMeetingReq = StopMeetingReq
waitForStopMeeting :: HasAppState s => FlowCallback s StopMeetingReq
waitForStopMeeting = requestComp isEvent
  where
    isEvent StopMeeting = Just StopMeetingReq
    isEvent _ = Nothing

data StartMeetingReq = StartMeetingReq
waitForStartMeeting :: HasAppState s => FlowCallback s StartMeetingReq
waitForStartMeeting = requestComp isEvent
  where
    isEvent StartMeeting = Just StartMeetingReq
    isEvent _ = Nothing

waitForInviteMember :: HasAppState s => FlowCallback s MemberData
waitForInviteMember = requestComp isEvent
  where
    isEvent (InviteMember memberData) = Just memberData
    isEvent _ = Nothing

type MemberID = Text
data MemberReq (reqType::MemberRequestType) = MemberReq MemberID

type ConnectMemberReq = MemberReq 'ConnectMember
type HoldMemberReq = MemberReq 'HoldMember
type MuteMemberReq = MemberReq 'MuteMember
type UnmuteMemberReq = MemberReq  'UnmuteMember
type DropMemberReq = MemberReq 'DropMember

waitForJoinBridge :: HasAppState s => FlowCallback s JoinBridgeParams
waitForJoinBridge = monitorCallbackWithTrans (subSvc.internReqDistr) getJoinBridgeParams
  where
    getJoinBridgeParams (JoinBridge params) = Just params
    getJoinBridgeParams _ = Nothing

waitForExitHandling :: HasAppState s => MemberID -> FlowCallback s ExitHandlingParams
waitForExitHandling mbrID= monitorCallbackWithTrans (subSvc.internReqDistr) getExitHandlingParams
  where
    getExitHandlingParams (ExitHandling params@(ExitHandlingParams mID)) = if (mbrID==mID) then Just params else Nothing
    getExitHandlingParams _ = Nothing

waitForMemberReq :: HasAppState s => (MemberRequestData -> Maybe rqType) -> MemberID -> FlowCallback s rqType
waitForMemberReq isRequest mbrID = requestComp reqConverter
  where
    reqConverter (MemberRequest reqData@(MemberRequestData mID _memReqType)) =
      if mID==mbrID
        then isRequest reqData
        else Nothing
    reqConverter _ = Nothing

waitForConnectMemberReq :: HasAppState s => MemberID -> FlowCallback s ConnectMemberReq
waitForConnectMemberReq = waitForMemberReq isConnectMember
  where
    isConnectMember (MemberRequestData mbrID ConnectMember) = Just $ MemberReq mbrID
    isConnectMember _ = Nothing

waitForHoldMemberReq :: HasAppState s => MemberID -> FlowCallback s HoldMemberReq
waitForHoldMemberReq = waitForMemberReq isHoldMember
  where
    isHoldMember (MemberRequestData mbrID HoldMember) = Just $ MemberReq mbrID
    isHoldMember _ = Nothing

waitForMuteMemberReq :: HasAppState s => MemberID -> FlowCallback s MuteMemberReq
waitForMuteMemberReq = waitForMemberReq isRequest
  where
    isRequest (MemberRequestData mbrID MuteMember) = Just $ MemberReq mbrID
    isRequest _ = Nothing

waitForUnmuteMemberReq :: HasAppState s => MemberID -> FlowCallback s UnmuteMemberReq
waitForUnmuteMemberReq = waitForMemberReq isRequest
  where
    isRequest (MemberRequestData mbrID UnmuteMember) = Just $ MemberReq mbrID
    isRequest _ = Nothing

waitForDropMemberReq :: HasAppState s => MemberID -> FlowCallback s DropMemberReq
waitForDropMemberReq = waitForMemberReq isRequest
  where
    isRequest (MemberRequestData mbrID DropMember) = Just $ MemberReq mbrID
    isRequest _ = Nothing

notifyAppEvent :: HasAppState s => TeamEvent -> FlowContext s ()
notifyAppEvent teamEvent = notifyEvent (subSvc.rsEventDistr) teamEvent

updateMembers :: HasAppState s => (MembersMap -> MembersMap) -> FlowContext s ()
updateMembers updateFunc = do
  newMembersMap <- subSvc.members <%= updateFunc
  notifyAppEvent $ MembersEvent $ Members newMembersMap

setMeetingState :: HasAppState s => MeetingState -> FlowContext s ()
setMeetingState newState = do
  subSvc.meetingState .= newState
  notifyAppEvent $ MeetingState newState

initMember :: HasAppState s => MemberData -> FlowContext s MemberID
initMember mbrData = do
  newIDInt <- (subSvc.lastMemberID) <%= (+1)
  let newID :: Text
      newID = show newIDInt
  updateMembers (M.insert newID (MemberEntry mbrData MemberInitiated))
  return newID

setMemberState :: HasAppState s => MemberID -> MemberState -> FlowContext s ()
setMemberState mbrID newState = updateMembers (adjust (\me-> me {memberState=newState}) mbrID)

removeMember :: HasAppState s => MemberID -> FlowContext s ()
removeMember mbrID = updateMembers (M.delete mbrID)

isAppStateEmpty :: HasAppState s => FlowContext s Bool
isAppStateEmpty =
    liftM3 allTrue (M.null <$> use (subSvc.members)) (Set.null <$> use (subSvc.talking)) ((==MeetingStateNull) <$> use (subSvc.meetingState))
  where
    allTrue fl1 fl2 fl3 = fl1 && fl2 && fl3

-- just to be on super safe state
resetAppState :: HasAppState s => FlowContext s ()
resetAppState =
  unlessM isAppStateEmpty $ do
    liftIO $ warningM loggerPath "state was supposed to be empty"
    setMeetingState MeetingStateNull
    updateMembers (const M.empty)

runMemberCall :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => MeetingConfig -> MemberData -> FlowContext s ()
runMemberCall config md = do
  let memberPhone = memberPhoneNum md
  clrID <- callerIDFromDialConfig memberPhone
  origChannelInitialized <- dialWithPlan  dialChannel clrID memberPhone
  mbrID <- initMember md
  _cc <- runCall (memberCall config md mbrID origChannelInitialized)
    (\_-> liftIO $ debugM loggerPath ("member call completed for memberID:" <> (show mbrID)))
  return ()

data ChannelDisconnected     = ChannelDisconnected deriving (Eq, Show)
data ShouldDisconnectChannel = ShouldDisconnectChannel deriving (Eq, Show)
data MemberPromptResult = MemberPromptAccepted | MemberPromptRejected deriving (Eq, Show)
data HandlingCompleted = HandlingCompleted deriving (Eq, Show)
yourConferenceWillBeginShortyPleaseHold :: PBMedia
yourConferenceWillBeginShortyPleaseHold = fileMedia "yourConferenceWillBeginShortyPleaseHold"
memberCall :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => MeetingConfig -> MemberData -> MemberID -> InitiatedChannel -> FlowCallback s ()
memberCall _config _md memberID initiatedChannel= do
    HandlingCompleted <- handleMemberCall
    debugMember "memberCall :: about to removeMember"
    lift $ removeMember memberID
  where
    standbyPauseTimeout = 1000000
    debugMember txt = debugLog $ txt <> ";memberID:" <> (show memberID)
    setState st = lift $ setMemberState memberID st
    waitForDropChannelReq :: (HasAppState s) => FlowCallback s ShouldDisconnectChannel
    waitForDropChannelReq = (const ShouldDisconnectChannel) <$> (waitForStopMeeting >||< (waitForDropMemberReq memberID))
    disconnectChannel = lift $ dropChannel initiatedChannel
    handleMemberCall :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => FlowCallback s HandlingCompleted
    handleMemberCall =
      do     void $ waitForChannelRinging initiatedChannel
             debugMember $ "handleMemberCall::waitForChannelRinging"
             setState MemberRinging
      <:| do debugMember "handleMemberCall::going to waitForChannelConnected"
             res <- (waitForDropChannelReq >||< waitForChannelConnected initiatedChannel)
             case res of
               Left ShouldDisconnectChannel -> disconnectChannel >> return HandlingCompleted
               Right connectedChannel ->
                do
                  debugMember "handleMemberCall::waitForChannelConnected triggered"
                  setState MemberAnswered
                  treatmentRes <- (waitForDropChannelReq -- (callWithOp waitForDropChannelReq (sendInternalReq (ExitHandling (ExitHandlingParams memberID))))
                                                >||< handleMemberConnected connectedChannel)
                  debugMember ("handleMemberCall::treatment completed: treatmentRes:" <> (show treatmentRes))
                  case treatmentRes of
                    Left ShouldDisconnectChannel -> spawnIVRComp playMeetingEnded connectedChannel >> return HandlingCompleted
                    Right (Right ShouldDisconnectChannel) -> spawnIVRComp playMeetingEnded connectedChannel >> return HandlingCompleted
                    Right (Left handlingCompleted) -> return handlingCompleted
      >|< do debugMember "handleMemberCall::going to waitForInitChannelTerminated"
             void $ waitForInitChannelTerminated initiatedChannel
             debugMember "handleMemberCall::waitForInitChannelTerminated"
             return HandlingCompleted
    -- handleMemberConnected :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => FlowCallback s ShouldDisconnectChannel
    playMeetingEnded :: IVRCallbackCont () ()
    playMeetingEnded = playList [filePB "MeetingEndedYouWillBeDisconnected"]
    playPostRejectedAnnouncement :: IVRCallbackCont () ()
    playPostRejectedAnnouncement = playList [filePB "WeAreSorryItDidnotWorkForYou",filePB "NextTimeMaybe",filePB "ThankYouAndGoodBye"]
    handleMemberConnected :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => ConnectedChannel -> FlowCallback s (Either HandlingCompleted ShouldDisconnectChannel)
    handleMemberConnected connectedChannel = do
       debugMember "handleMemberCall::handleMemberConnected entered"
       promptResult <- promptMember connectedChannel
       debugMember $ "handleMemberCall::handleMemberConnected entered; promptResult" <> show promptResult
       case promptResult of
         MemberPromptRejected ->
           spawnIVRComp playPostRejectedAnnouncement connectedChannel >> (return $ Left HandlingCompleted)
         MemberPromptAccepted -> Right <$> handleStandbyChannel connectedChannel
    promptMember connectedChannel = do
      setState MemberIntro
      debugMember "handleMemberCall::promptMember going to promptMemberToJoin"
      evalIVRComp' promptMemberToJoin connectedChannel
    promptMemberToJoin = do
      ivrRes <- createIVRMenu
                  ["YouHaveBeenCalledToJoinTheMeeting"
                  ,"PressOneToJoinOtTwoIfYouUnable"
                  ]
                  [DTMF_1
                  ,DTMF_2
                  ]
      let appRes=case ivrRes of
                    Nothing ->       MemberPromptRejected
                    Just DTMF_2 ->   MemberPromptRejected
                    Just DTMF_1 ->   MemberPromptAccepted
                    _ ->             MemberPromptRejected
      return appRes
    handleStandbyChannel connectedChannel = do
      debugMember "handleMemberCall::handleStandbyChannel entered"
      (JoinBridgeParams bridgeHandle)<- (callWithOp waitForJoinBridge (setMemberState memberID MemberStandBy)) |:>
        handleWaitStandby connectedChannel
      connectAndHandleMember connectedChannel bridgeHandle
    handleWaitStandby connectedChannel = do
      debugMember "handleMemberCall::handleWaitStandby entered"
      timeCallback standbyPauseTimeout
      PlaySoundCompleted <- playSoundOnChannelComp yourConferenceWillBeginShortyPleaseHold connectedChannel
      playMOHChannelComp connectedChannel
    connectAndHandleMember connectedChannel bridgeHandle = do
      debugMember "handleMemberCall::connectAndHandleMember entered"
      setState (MemberConnected $ MemberConnectedState {muted=False})
      bridgeChannelResource bridgeHandle connectedChannel >|>
        waitForHoldMemberReq memberID |:>
            handleMuteUnmute connectedChannel -- >||< handleTalkDetection
      >> holdAndHandleMember connectedChannel bridgeHandle
    holdAndHandleMember connectedChannel bridgeChannel=
      debugMember "handleMemberCall::holdAndHandleMember entered"
      >> setState MemberOnHold
      >> newMOHChannelResource connectedChannel >|>
        waitForConnectMemberReq memberID
      >> connectAndHandleMember connectedChannel bridgeChannel
    handleMuteUnmute connectedChannel =
      debugMember "handleMemberCall::handleMuteUnmute entered"
      >> waitForMuteMemberReq memberID
      >> muteChannelResource MuteDirIn connectedChannel >:>
         (  debugMember "handleMemberCall::handleMuteUnmute mute sent"
         >> setState (MemberConnected $ MemberConnectedState {muted=True})
         >> debugMember "handleMemberCall::handleMuteUnmute muted"
         >> waitForUnmuteMemberReq memberID
         >> debugMember "handleMemberCall::handleMuteUnmute unmute received")
      >> setState (MemberConnected $ MemberConnectedState {muted=False})
      >> handleMuteUnmute connectedChannel

addMemberHandler :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => MeetingConfig -> FlowCallback s ()
addMemberHandler config = forever $ do
  md <- waitForInviteMember
  lift $ runMemberCall config md

data MeetingShouldEnd = MeetingShouldEnd
data MeetingShouldStart = MeetingShouldStart
manageMeeting :: (HasAppState s , HasDialConfig s, HasPJSIPAccount s, HasARIEventDistributor s) => MeetingConfig -> FlowCallback s MeetingShouldEnd
manageMeeting config = do
    debugLog "manageMeeting entered"
    MeetingShouldStart <- waitForMeetingShoudStart config
    debugLog "manageMeeting : MeetingShouldStart"
    withResource newBridgeResource $ \bridgeHandle -> do
      sendConnectToBridgeReq bridgeHandle
      waitForMeetingShoudEnd config bridgeHandle

standByMembers :: Members -> [MemberEntry]
standByMembers (Members membersMap) = L.filter (\me-> memberState me == MemberStandBy) (mapValues membersMap)

canMeetingBeGoing :: [MemberEntry] -> MeetingConfig -> Bool
canMeetingBeGoing members config =
  -- debugLog $ "canMeetingBeGoing; members to consider: " <> (show members)
  let memberRoles = fmap (\me-> memberRole (memberData me)) members
  in case startMode config of
    StartManually   -> False
    AnyMember       -> (not . L.null) memberRoles
    MustParticipant -> isJust $ L.find (\role-> role==Participant || role==Anchor) memberRoles
    MustAnchor      -> isJust $ L.find (==Anchor) memberRoles

canMeetingBeGoing' :: Members -> MeetingConfig -> Bool
canMeetingBeGoing' (Members membersMap) = canMeetingBeGoing (mapValues membersMap)

canMeetingStart :: Members -> MeetingConfig -> Bool
canMeetingStart members   = canMeetingBeGoing (standByMembers members)

waitForMeetingCanStartBasedOnMembersState :: (HasAppState s) => MeetingConfig -> FlowCallback s MeetingShouldStart
waitForMeetingCanStartBasedOnMembersState config = monitorCallbackWithTrans (subSvc.rsEventDistr) getMembersEvent
  where
    getMembersEvent (MembersEvent members) = if (canMeetingStart members config) then (Just MeetingShouldStart) else Nothing
    getMembersEvent _ = Nothing

waitForMeetingShoudStart :: HasAppState s => MeetingConfig -> FlowCallback s MeetingShouldStart
waitForMeetingShoudStart config = do
  debugLog "waitForMeetingShoudStart entered"
  waitForStartMeeting>><StartMeetingReq >|< waitForMeetingCanStartBasedOnMembersState config>><MeetingShouldStart
  debugLog "waitForMeetingShoudStart MeetingShouldStart"
  return MeetingShouldStart

handleMeetingInProgess  :: HasAppState s => MeetingConfig -> BridgeHandle -> (Members -> MeetingConfig -> Bool) -> FlowCallback s ()
handleMeetingInProgess config brdHandle whilePred = doWhile $ do
  membersData <- waitForMembersEvent
  debugLog $ "waitForMeetingShoudStart got members event:" <> (show membersData)
  let hasStandByMembers :: Bool
      hasStandByMembers = (not . L.null) (standByMembers membersData)
  debugLog $ "waitForMeetingShoudStart has StandBy members:" <> (show hasStandByMembers)
  when hasStandByMembers (sendConnectToBridgeReq brdHandle)
  let flContinue = whilePred membersData config
  debugLog $ "waitForMeetingShoudStart :: continue:" <> (show flContinue)
  return flContinue

waitForMeetingShoudEnd :: HasAppState s => MeetingConfig -> BridgeHandle -> FlowCallback s MeetingShouldEnd
waitForMeetingShoudEnd config brdHandle = do
  debugLog "waitForMeetingShoudEnd entered"
  lift $ setMeetingState MeetingStateActive
  handleMeetingInProgess config brdHandle canMeetingBeGoing'
  lift $ setMeetingState MeetingStateGrace
  debugLog "waitForMeetingShoudEnd started grace period"
  res <- timeCallback ((stopGraceTimeoutSec config) * 1000000) >||<
    handleMeetingInProgess config brdHandle (\mbrs cfg -> not $ canMeetingBeGoing' mbrs cfg) -- wait for meeting recovery
  debugLog $ "waitForMeetingShoudEnd grace period handling completed" <> (show res)
  case res of
    Left ()   -> return MeetingShouldEnd
    Right ()  -> waitForMeetingShoudEnd config brdHandle

sendConnectToBridgeReq :: HasAppState s => BridgeHandle -> FlowCallback s ()
sendConnectToBridgeReq brdHandle = lift $ sendInternalReq (JoinBridge $ JoinBridgeParams brdHandle)

doTeamCall :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => FlowCallback s ()
doTeamCall = do
  debugLog "doTeamCall entered"
  --lift resetAppState

  initMeetingParams <- waitForInitMeeting
  debugLog  $ "doTeamCall : initMeeting received; params:" <> (show initMeetingParams)
  lift $ setMeetingState MeetingStateInit

  let config = meetingConfig initMeetingParams
  --start calls for all members in initMeetingParams
  forM_ (meetingMembers initMeetingParams) (\memberData -> lift $ runMemberCall config memberData)

  debugLog  "doTeamCall : going to waitForStopMeeting"
  waitForStopMeeting >><StopMeetingReq  >|< manageMeeting config >><MeetingShouldEnd |:> addMemberHandler config
  lift $ setMeetingState MeetingStateNull

  debugLog "doTeamCall finished"

teamCallApp :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => FlowCallback s ()
teamCallApp = forever doTeamCall
