{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FlowAppComps.BasicCallApp
  ( BasicCallReq (..)
  , DestParty (..)
  , BasicCallRS (..)
  , AppState
  , HasAppState (..)
  , basicCallApp
  ) where

import Protolude
import Prelude (String)
-- import Data.Aeson hiding ((.=), defaultOptions)
import Control.Lens
import Data.Text
import Elm.Derive
-- import Elm.Module
-- import Data.Proxy

import FlowAppComps.DialingPlan
import FlowAppComps.FlowAppHelpers

import Utils
import ARICore
import ARIUtils
import FlowTools

loggerPath::String
loggerPath=getLoggerPath "BasicCallApp"

data DestParty = DestParty {destNum :: Text, destName :: Maybe Text} deriving (Show, Eq, Generic)
--instance ToJSON DestParty
--instance FromJSON DestParty
deriveBoth defaultOptions ''DestParty

data BasicCallReq = MakeCall DestParty  | Dial DestParty | Hangup | DropCall  deriving (Show, Eq, Generic)
--instance ToJSON BasicCallReq
--instance FromJSON BasicCallReq
deriveBoth defaultOptions ''BasicCallReq

{-
--data MakeCallParams = MakeCallParams {mkCallDestNum :: Text, mkCallDestName :: Text}  deriving (Show, Eq, Generic)
--data DialParams =     DialParams Text deriving (Show, Eq, Generic)
--data HangupParams =   HangupParams deriving (Show, Eq, Generic)
--data DropCallParams = DropCallParams deriving (Show, Eq, Generic)


instance ToJSON MakeCallParams
instance FromJSON MakeCallParams

instance ToJSON DialParams
instance FromJSON DialParams
-}

data BasicCallRS = NoCall
                 | Initiated DestParty
                 | RingingCaller DestParty
                 | CallerIdle
                 | InitiatedCallee DestParty
                 | RingingCallee DestParty
                 | Connected DestParty deriving (Show, Eq, Generic)

-- instance ToJSON BasicCallRS
-- instance FromJSON BasicCallRS
deriveBoth defaultOptions ''BasicCallRS

data AppState s = AppState
        { _stateRS :: BasicCallRS --BasicCallState
        , _rsEventDistr :: EventDistributor BasicCallRS (FlowContextM s) s
        , _reqDistr :: EventDistributor BasicCallReq (FlowContextM s) s
        }

makeLenses ''AppState

instance EmptyData (AppState s) where
  emptyData = AppState NoCall newEventDistributor newEventDistributor

--type HasAppState s = SubSvc (AppState s) s
class HasAppState s where
  subSvc :: Lens' s (AppState s)

instance FlowSubSvc BasicCallRS BasicCallReq (AppState s) s where
  reqDisp = reqDistr
  evDistr = rsEventDistr

instance GetSnapshot (AppState s) BasicCallRS  where
  snapshot = _stateRS

requestComp :: (HasAppState s) => (BasicCallReq-> Maybe rq) -> FlowCallback s rq
requestComp = monitorCallbackWithTrans (subSvc.reqDistr)  -- transReq

waitForMakeCall :: HasAppState s => FlowCallback s DestParty
waitForMakeCall = requestComp isEvent
  where
    isEvent (MakeCall params) = Just params
    isEvent _ = Nothing
waitForDial :: HasAppState s => FlowCallback s DestParty
waitForDial = requestComp isEvent
  where
    isEvent (Dial params) = Just params
    isEvent _ = Nothing

data HangupReq = HangupReq
waitForHangup :: HasAppState s => FlowCallback s HangupReq
waitForHangup = requestComp isEvent
  where
    isEvent Hangup = Just HangupReq
    isEvent _ = Nothing

data DropCallReq = DropCallReq
waitForDropCall :: HasAppState s => FlowCallback s DropCallReq
waitForDropCall = requestComp isEvent
  where
    isEvent DropCall = Just DropCallReq
    isEvent _ = Nothing

setStateAndNotify :: HasAppState s => BasicCallRS -> FlowCallback s ()
setStateAndNotify callRS = do
  curState <- use (subSvc.stateRS)
  debugLog $ "Move from state: [" <> show curState <> "] to state: [" <> show callRS <> "]"
  (subSvc.stateRS) .= callRS
  lift $ notifyEvent (subSvc.rsEventDistr) callRS

debugLog :: Text -> FlowCallback s ()
debugLog txt = liftIO $ debugM loggerPath (unpack txt)

basicCallApp :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => FlowCallback s ()
basicCallApp = startBasicCall

startBasicCall :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => FlowCallback s ()
startBasicCall = do
  debugLog "Entered startBasicCall"
  setStateAndNotify NoCall
  destParty <- waitForMakeCall
  debugLog $ "Got makeCall request: " <> show destParty
  callerPhone <- lift phoneNumFromDialConfig
  origChannelInitialized <- lift $ dialWithPlan  dialChannel (destNum destParty) callerPhone
  setStateAndNotify (Initiated destParty)
  res <-raceCall waitForHangup (callingCallee origChannelInitialized callerPhone destParty)
  case res of
    Left HangupReq -> do
      debugLog  "HangupReq detected"
      lift $ dropChannel origChannelInitialized
      startBasicCall
    Right CalleeTerminated -> do
      debugLog  "CalleeTerminated"
      startBasicCall
  return ()

data CalleeTerminated = CalleeTerminated deriving Show
data DestTerminated = DestTerminated deriving Show

callingCallee :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => InitiatedChannel -> Text -> DestParty ->  FlowCallback s CalleeTerminated
callingCallee origChannelInitialized calllerPhone destParty = do
  debugLog $ "Entered callingCallee calllerPhone:" <> calllerPhone <> " dest:" <> show destParty

  do     void $ waitForChannelRinging origChannelInitialized
         debugLog "callingCallee::waitForChannelRinging"
         setStateAndNotify (RingingCaller destParty)
  <:| do debugLog "callingCallee::going to waitForChannelConnected"
         connectedChannel <- waitForChannelConnected origChannelInitialized
         debugLog "callingCallee::waitForChannelConnected"
         handleCallerConnected connectedChannel calllerPhone destParty
  >|< do debugLog "callingCallee::going to waitForInitChannelTerminated"
         void $ waitForInitChannelTerminated origChannelInitialized
         debugLog "callingCallee::waitForInitChannelTerminated"
         return CalleeTerminated

handleCallerConnected :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => ConnectedChannel -> Text -> DestParty ->  FlowCallback s CalleeTerminated
handleCallerConnected origChannelConnected callerPhone destParty = do
  debugLog "handleCallerConnected entered"
  DestTerminated <- dialDestAndBridgeBoth origChannelConnected destParty
  debugLog "handleCallerConnected::DestTerminated"
  idleRes <- handleCalleeIdle
  debugLog $ "handleCallerConnected::idleRes:" <> show idleRes
  case idleRes of
    Nothing -> lift $ dropChannel origChannelConnected >> return CalleeTerminated
    Just newDest -> handleCallerConnected origChannelConnected callerPhone newDest

handleCalleeIdle :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => FlowCallback s (Maybe DestParty) -- if collected return new dest
handleCalleeIdle = do
  debugLog "handleCalleeIdle entered"
  setStateAndNotify CallerIdle
  resIdle <-  timeCallback 1200000000 >||< waitForDial
  debugLog $ "handleCalleeIdle::idleRes:" <> show resIdle
  case resIdle of
    Left () -> return Nothing
    Right destParty -> return $ Just destParty

dialDestAndBridgeBoth :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s, HasDialConfig s) => ConnectedChannel -> DestParty ->  FlowCallback s DestTerminated
dialDestAndBridgeBoth origChannelConnected destParty = do
  clrID <- lift $ callerIDFromDialConfig (destNum destParty)
  debugLog $ "dialDestAndBridgeBoth callerID:"<> clrID
  destChannelInitialized <- lift $ dialWithPlan  dialChannel clrID (destNum destParty)
  onCancel_ (dropChannel destChannelInitialized) $ do
    setStateAndNotify (InitiatedCallee destParty)
    res <- waitForDropCall >||< do
      destChannelConnectedMaybe <- handleConnectingToDest origChannelConnected destChannelInitialized destParty
      case destChannelConnectedMaybe of
        Nothing -> return DestTerminated
        Just destChannelConnected ->
          (waitForChannelTerminatedEvent destChannelConnected >||< handleBothChannelsConnected origChannelConnected destChannelConnected destParty) >> return DestTerminated
    case res of
      Left DropCallReq -> lift $ dropChannel destChannelInitialized >> return DestTerminated
      Right DestTerminated -> return DestTerminated

handleConnectingToDest :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => ConnectedChannel -> InitiatedChannel -> DestParty -> FlowCallback s (Maybe ConnectedChannel)
handleConnectingToDest origChannelConnected destChannelInitialized destParty = do
  debugLog "handleConnectingToDest entered"
  withResource_ (newRingChannelResource origChannelConnected) $
    waitForChannelRinging destChannelInitialized >> do
        debugLog "handleConnectingToDest::waitForChannelRinging"
        setStateAndNotify (RingingCallee destParty)
    <:| (waitForInitChannelTerminated destChannelInitialized >||< waitForChannelConnected destChannelInitialized ) >>= \outBoundConnectedRes -> case outBoundConnectedRes of
        Left ChannelTerminated -> do
          debugLog "handleConnectingToDest::Call to Caller failed after ringing"
          return Nothing
        Right connectedCalledChannel -> do
          debugLog "handleConnectingToDest::Call to Caller connected"
          return $ Just connectedCalledChannel


handleBothChannelsConnected :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => ConnectedChannel -> ConnectedChannel -> DestParty -> FlowCallback s DestTerminated
handleBothChannelsConnected origChannelConnected destChannelConnected destParty = do
  debugLog "handleBothChannelsConnected entered"
  setStateAndNotify (Connected destParty)
  withResource newBridgeResource $ \bridgeHandle -> lift (do
    addChannelToBridge bridgeHandle origChannelConnected
    addChannelToBridge bridgeHandle destChannelConnected) >> timeCallback 3600000000 >> lift (dropChannel destChannelConnected) >> return DestTerminated -- max call duration
