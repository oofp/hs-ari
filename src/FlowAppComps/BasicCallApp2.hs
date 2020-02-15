{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FlowAppComps.BasicCallApp2
  ( BasicCallReq2 (..)
  , BasicCallRS2 (..)
  , AppState2
  , HasAppState (..)
  , basicCallApp2
  ) where

import Protolude
import Prelude (String)
import Control.Lens

import Elm.Derive

import FlowAppComps.DialingPlan

import Utils
import ARICore
import ARIUtils
import FlowTools

loggerPath::String
loggerPath=getLoggerPath "BasicCallApp"

data MakeCallParams = MakeCallParams Text Text deriving (Show, Eq, Generic)
data DialParams =     DialParams Text deriving (Show, Eq, Generic)

data BasicCallReq2 = MakeCall MakeCallParams  | Dial DialParams | Hangup | DropCall  deriving (Show, Eq, Generic)

--instance ToJSON MakeCallParams
--instance FromJSON MakeCallParams
deriveBoth defaultOptions ''MakeCallParams

--instance ToJSON DialParams
--instance FromJSON DialParams
deriveBoth defaultOptions ''DialParams

--instance ToJSON BasicCallReq2
--instance FromJSON BasicCallReq2
deriveBoth defaultOptions ''BasicCallReq2

data BasicCallRS2 = NoCall | Initiated | RingingCaller | CallerIdle | InitiatedCallee | RingingCallee | Connected deriving (Show, Eq, Generic)

--instance ToJSON BasicCallRS2
--instance FromJSON BasicCallRS2
deriveBoth defaultOptions ''BasicCallRS2

{-
data BasicCallState = NoCallState
                    | InitiatedState InitiatedChannel
                    | RingingCCallerState RingingChannel
                    | CallerIdleState ConnectedChannel
                    | InitiatedCalleeState ConnectedChannel InitiatedChannel
                    | RingingCalleeState ConnectedChannel RingingChannel
                    | ConnectedState ConnectedChannel ConnectedChannel
-}

data AppState2 s = AppState2
        { _stateRS :: BasicCallRS2 --BasicCallState
        , _rsEventDistr :: EventDistributor BasicCallRS2 (FlowContextM s) s
        , _reqDistr :: EventDistributor BasicCallReq2 (FlowContextM s) s
        }

makeLenses ''AppState2

instance EmptyData (AppState2 s) where
  emptyData = AppState2 NoCall newEventDistributor newEventDistributor

class HasAppState s where
  subSvc :: Lens' s (AppState2 s)

instance (HasAppState s) => FlowSubSvc BasicCallRS2 BasicCallReq2 (AppState2 s) s where
  reqDisp = reqDistr
  evDistr = rsEventDistr


requestComp :: (HasAppState s) => (BasicCallReq2-> Maybe rq) -> FlowCallback s rq
requestComp = monitorCallbackWithTrans (subSvc.reqDistr)  -- transReq

waitForMakeCall :: HasAppState s => FlowCallback s MakeCallParams
waitForMakeCall = requestComp isEvent
  where
    isEvent (MakeCall params) = Just params
    isEvent _ = Nothing
waitForDial :: HasAppState s => FlowCallback s DialParams
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


setStateAndNotify :: HasAppState s => BasicCallRS2 -> FlowCallback s ()
setStateAndNotify callRS = do
  (subSvc.stateRS) .= callRS
  lift $ notifyEvent (subSvc.rsEventDistr) callRS

basicCallApp2 :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => FlowCallback s ()
basicCallApp2 = startBasicCall

startBasicCall :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => FlowCallback s ()
startBasicCall = do
  setStateAndNotify NoCall
  (MakeCallParams callerPhone dest) <- waitForMakeCall
  origChannelInitialized <- lift $ dialWithPlan  dialChannel dest callerPhone
  setStateAndNotify Initiated
  res <-raceCall waitForHangup (callingCallee origChannelInitialized callerPhone dest)
  case res of
    Left HangupReq -> do
      lift $ dropChannel origChannelInitialized
      startBasicCall
    Right CalleeTerminated ->
      startBasicCall
  return ()

data CalleeTerminated = CalleeTerminated
data DestTerminated = DestTerminated

callingCallee :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => InitiatedChannel -> Text -> Text ->  FlowCallback s CalleeTerminated
callingCallee origChannelInitialized calllerPhone dest = do
  callProgress <- waitForCallProgressEvent origChannelInitialized
  case callProgress of
    CallProgressTerminated _ -> liftIO $ infoM loggerPath "Call to Caller failed" >> return CalleeTerminated
    CallProgressConnected connectedChannel -> handleCallerConnected connectedChannel calllerPhone dest
    CallProgressRinging ringingChannel -> do
      setStateAndNotify RingingCaller
      callConnectedRes <- waitForCallConnectedEvent ringingChannel
      case callConnectedRes of
        Left ChannelTerminated  -> liftIO $ infoM loggerPath "Call to Caller failed after ringing" >> return CalleeTerminated
        Right connectedChannel -> handleCallerConnected connectedChannel calllerPhone dest

handleCallerConnected :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => ConnectedChannel -> Text -> Text ->  FlowCallback s CalleeTerminated
handleCallerConnected origChannelConnected callerPhone dest = do
  res <- raceCall (waitForChannelTerminatedEvent origChannelConnected) (dialDestAndBridgeBoth origChannelConnected callerPhone dest)
  case res of
    Left ChannelTerminated -> return CalleeTerminated
    Right DestTerminated -> do
      idleRes <- handleCalleeIdle
      case idleRes of
        Nothing -> lift $ dropChannel origChannelConnected >> return CalleeTerminated
        Just newDest -> handleCallerConnected origChannelConnected callerPhone newDest

handleCalleeIdle :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => FlowCallback s (Maybe Text) -- if collected return new dest
handleCalleeIdle = do
  setStateAndNotify CallerIdle
  resIdle <- raceCall (timeCallback 1200000000) waitForDial
  case resIdle of
    Left () -> return Nothing
    Right (DialParams newDest) -> return $ Just newDest

dialDestAndBridgeBoth :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => ConnectedChannel -> Text -> Text ->  FlowCallback s DestTerminated
dialDestAndBridgeBoth origChannelConnected callerPhone dest = do
  destChannelInitialized <- lift $ dialWithPlan  dialChannel callerPhone dest
  onCancel (\_ -> dropChannel destChannelInitialized) $ do
    setStateAndNotify InitiatedCallee
    res <- raceCall waitForDropCall (do
      destChannelConnectedMaybe <- handleConnectingToDest origChannelConnected destChannelInitialized
      case destChannelConnectedMaybe of
        Nothing -> return DestTerminated
        Just destChannelConnected -> handleBothChannelsConnected origChannelConnected destChannelConnected)
    case res of
      Left DropCallReq -> lift $ dropChannel destChannelInitialized >> return DestTerminated
      Right DestTerminated -> return DestTerminated

handleConnectingToDest :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => ConnectedChannel -> InitiatedChannel ->  FlowCallback s (Maybe ConnectedChannel)
handleConnectingToDest origChannelConnected destChannelInitialized =
  withResource_ (newRingChannelResource origChannelConnected) $ do
    outBoundCallProgress <- waitForCallProgressEvent destChannelInitialized
    case outBoundCallProgress of
      CallProgressTerminated _  -> return Nothing
      CallProgressConnected connectedCalledChannel -> return $ Just connectedCalledChannel
      CallProgressRinging ringingCalledChannel -> do
        setStateAndNotify RingingCallee
        outBoundConnectedRes <- waitForCallConnectedEvent ringingCalledChannel
        case outBoundConnectedRes of
          Left ChannelTerminated -> liftIO $ infoM loggerPath "Call to Caller failed after ringing" >> return Nothing
          Right connectedCalledChannel -> return $ Just connectedCalledChannel


handleBothChannelsConnected :: (HasAppState s, HasARIEventDistributor s, HasPJSIPAccount s) => ConnectedChannel -> ConnectedChannel ->  FlowCallback s DestTerminated
handleBothChannelsConnected origChannelConnected destChannelConnected = do
  setStateAndNotify Connected
  withResource newBridgeResource $ \bridgeHandle -> lift (do
    addChannelToBridge bridgeHandle origChannelConnected
    addChannelToBridge bridgeHandle destChannelConnected) >> timeCallback 3600000000 >> lift (dropChannel destChannelConnected) >> return DestTerminated -- max call duration
