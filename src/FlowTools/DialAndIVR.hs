{-# LANGUAGE DataKinds #-}

module FlowTools.DialAndIVR
  ( dialAndIVR
  , dialIVRAndDisc
  , dialAndDisc
  , dialFlowAndDisc
  , DialAndDiscRes (..)
  ) where

import Protolude

import Utils
import ARICore
import ARIUtils
import FlowTools.FlowCallbackComps
import FlowTools.IVRComps
import FlowTools.FlowEvDistr
import FlowTools.FlowCallback
import FlowTools.TimerTool
import Prelude (String)

loggerPath::String
loggerPath=getLoggerPath "DialAndIVR"

dialAndIVR :: (HasARIEventDistributor s, EmptyData s1) => Tech -> Provider -> Party -> Party -> IVRCallbackCont s1 a -> FlowCallback s (Maybe ((a,s1), ConnectedChannel, FlowCallback s ChannelTerminated))
dialAndIVR tech prov callerID destParty ivrComp =
    dialChannelConnectedComp tech prov destParty callerID `ifJustMaybe` (\(connectedChannel, channelDiscMon) -> do
        ivrRes <- oneOfCall' (runIVRComp ivrComp connectedChannel) channelDiscMon
        case ivrRes of
          (FirstCompleted' res discMonitor) -> return $ Just (res, connectedChannel, discMonitor)
          (SecondCompleted' _)  -> return Nothing)

dialIVRAndDisc :: (HasARIEventDistributor s, EmptyData s1) => Tech -> Provider -> Party -> Party -> IVRCallbackCont s1 a -> FlowCallback s (Maybe (a,s1))
dialIVRAndDisc tech prov callerID destParty ivrComp =
  dialAndIVR tech prov callerID destParty ivrComp `ifJust` (\(res, connectedChannel, _) -> lift $ dropChannel connectedChannel >> return res)

data DialAndDiscRes = DDR_FAILED | DDR_LOCAL_DISC | DDR_REMOTE_DISC

dialAndDisc :: (HasARIEventDistributor s) => Tech -> Provider -> Party -> Party -> Integer -> FlowCallback s DialAndDiscRes
dialAndDisc tech prov callerID destParty maxCallDuration = do
  dialRes <- dialChannelConnectedComp tech prov destParty callerID
  case dialRes of
    Nothing -> return DDR_FAILED
    Just (connectedChannel, waitForDisc) -> do
      timeoutRes <- waitForDisc >||< timeCallback maxCallDuration
      case timeoutRes of
        Left ChannelTerminated  ->  return DDR_REMOTE_DISC
        Right () -> lift (dropChannel connectedChannel) >> return DDR_LOCAL_DISC

dialFlowAndDisc :: (HasARIEventDistributor s) => Tech -> Provider -> Party -> Party -> (ConnectedChannel -> FlowCallback s ()) -> FlowCallback s DialAndDiscRes
dialFlowAndDisc tech prov callerID destParty flowCmd = do
  dialRes <- dialChannelConnectedComp tech prov destParty callerID
  case dialRes of
    Nothing -> return DDR_FAILED
    Just (connectedChannel, waitForDisc) -> do
      myid <- liftIO myThreadId
      liftIO $ debugM loggerPath ("dialFlowAndDisc  tID:" <> show myid)
      timeoutRes <- waitForDisc >||< flowCmd connectedChannel
      case timeoutRes of
        Left ChannelTerminated  ->  return DDR_REMOTE_DISC
        Right () -> lift (dropChannel connectedChannel) >> return DDR_LOCAL_DISC
