{-# LANGUAGE DataKinds #-}

module FlowAppComps.CallbackComp
  ( callbackComp
  ) where

import Protolude
import Control.Monad
import Prelude (String)

import Utils
import ARICore
import ARIUtils
import FlowTools

loggerPath::String
loggerPath=getLoggerPath "CallbackComp"

callbackComp :: HasARIEventDistributor s => Tech -> Provider -> Party -> Party -> FlowCallback s ()
callbackComp tech prov callerID destParty = do
    dialCallerRes <- dialChannelComp tech prov destParty callerID
    case dialCallerRes of
      Res3 ChannelTerminated -> liftIO $ infoM loggerPath "Call to Caller failed"
      Res2 connectedCallerChannel callerDiscMonitor -> handleCallerConnected connectedCallerChannel callerDiscMonitor
      Res1 _ringingCallerChannel callerConnectedDiscMonitor ->
        do waitForConnectedRes <- callerConnectedDiscMonitor
           case waitForConnectedRes of
             SecondCompleted' ChannelTerminated -> liftIO $ infoM loggerPath "Call to Caller failed after ringing"
             FirstCompleted' connectedCallerChannel callerDiscMonitor -> handleCallerConnected connectedCallerChannel callerDiscMonitor

  where
    handleCallerConnected :: HasARIEventDistributor s => ConnectedChannel -> FlowCallback s ChannelTerminated -> FlowCallback s ()
    handleCallerConnected connectedChannel callerDiscMonitor = void $ oneOfCall' callerDiscMonitor (tryDialCalledParyAndHandleResult destParty)
      where
        tryDialCalledParyAndHandleResult dest = do
          void $ playSoundOnChannelComp (PBSound $ PBSoundParams "Connecting") connectedChannel
          res <- callDest connectedChannel dest
          let soundsToPlay = case res of
                                          CallToDestTermNormally -> ["ThankYouAndGoodBye"]
                                          CallToDestFailed ->       ["SorryTheNumberYouHaveDialedBusyOrUnavail" , "ThankYouAndGoodBye"]
          mapM_ (\sound -> playSoundOnChannelComp (PBSound $ PBSoundParams sound) connectedChannel) soundsToPlay
          lift $ dropChannel connectedChannel

    callDest :: HasARIEventDistributor s => ConnectedChannel -> Party -> FlowCallback s CallToDestResult
    callDest connectedChannel destForThisCall = do
      destChannelInitialized <- lift $ dialChannel tech prov callerID destForThisCall
      onCancel (\_-> dropChannel destChannelInitialized) $ do -- to drop dest call if orig call dropped
        dialOutRes <- withResource_ (newRingChannelResource connectedChannel) $ do
          outBoundCallRes <- monitorOutboundCall destChannelInitialized
          case outBoundCallRes of
            Res3 ChannelTerminated -> return Nothing
            Res2 connectedCalledChannel calledDiscMonitor -> return $ Just (connectedCalledChannel, calledDiscMonitor)
            Res1 _ringingCalledChannel calledConnectedDiscMonitor -> do
              waitForConnectedRes <- calledConnectedDiscMonitor
              case waitForConnectedRes of
                SecondCompleted' ChannelTerminated -> liftIO $ infoM loggerPath "Call to Called party failed after ringing" >> return Nothing
                FirstCompleted' connectedCalledChannel calledDiscMonitor -> return $ Just (connectedCalledChannel, calledDiscMonitor)
        case dialOutRes of
          Nothing -> return CallToDestFailed
          Just (connectedCalledChannel, calledDiscMonitor) -> handleBothConnected connectedChannel connectedCalledChannel calledDiscMonitor >> return CallToDestTermNormally

    handleBothConnected :: HasARIEventDistributor s => ConnectedChannel -> ConnectedChannel -> FlowCallback s ChannelTerminated -> FlowCallback s ()
    handleBothConnected connectedCallerChannel connectedCalledChannel calledTermMonitor =
      void $ oneOfCall' (handleBothConnected' connectedCallerChannel connectedCalledChannel) calledTermMonitor

    handleBothConnected' :: HasARIEventDistributor s => ConnectedChannel -> ConnectedChannel -> FlowCallback s ()
    handleBothConnected' connectedCallerChannel connectedCalledChannel =
      withResource newBridgeResource $ \bridgeHandle -> (lift $ do
        addChannelToBridge bridgeHandle connectedCalledChannel
        addChannelToBridge bridgeHandle connectedCallerChannel) >> timeCallback 3600000000 -- max call duration


data CallToDestResult = CallToDestTermNormally | CallToDestFailed
