{-# LANGUAGE DataKinds #-}

module FlowAppComps.CallbackComp2
  ( callbackComp
  ) where

--import Prelude (String)
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
    origChannelInitialized <- lift $ dialChannel tech prov destParty callerID
    callProgress <- waitForCallProgressEvent origChannelInitialized
    case callProgress of
      CallProgressTerminated _ -> liftIO $ infoM loggerPath "Call to Caller failed"
      CallProgressConnected connectedChannel -> handleCallerConnected connectedChannel
      CallProgressRinging ringingChannel -> do
        callConnectedRes <- waitForCallConnectedEvent ringingChannel
        case callConnectedRes of
          Left ChannelTerminated  -> liftIO $ infoM loggerPath "Call to Caller failed after ringing"
          Right connectedChannel -> handleCallerConnected connectedChannel

  where
    handleCallerConnected :: HasARIEventDistributor s => ConnectedChannel -> FlowCallback s ()
    handleCallerConnected connectedChannel = void $ raceCall (waitForChannelTerminatedEvent connectedChannel) (tryDialCalledParyAndHandleResult destParty)
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
          outBoundCallProgress <- waitForCallProgressEvent destChannelInitialized
          case outBoundCallProgress of
            CallProgressTerminated _  -> return Nothing
            CallProgressConnected connectedCalledChannel -> return $ Just connectedCalledChannel
            CallProgressRinging ringingCalledChannel -> do
              outBoundConnectedRes <- waitForCallConnectedEvent ringingCalledChannel
              case outBoundConnectedRes of
                Left ChannelTerminated -> liftIO $ infoM loggerPath "Call to Caller failed after ringing" >> return Nothing
                Right connectedCalledChannel -> return $ Just connectedCalledChannel
        case dialOutRes of
          Nothing -> return CallToDestFailed
          Just connectedCalledChannel -> handleBothConnected connectedChannel connectedCalledChannel >> return CallToDestTermNormally

    handleBothConnected :: HasARIEventDistributor s => ConnectedChannel -> ConnectedChannel -> FlowCallback s ()
    handleBothConnected connectedCallerChannel connectedCalledChannel =
      void $ raceCall (handleBothConnected' connectedCallerChannel connectedCalledChannel) (waitForChannelTerminatedEvent connectedCalledChannel)

    handleBothConnected' :: HasARIEventDistributor s => ConnectedChannel -> ConnectedChannel -> FlowCallback s ()
    handleBothConnected' connectedCallerChannel connectedCalledChannel =
      withResource newBridgeResource $ \bridgeHandle -> lift (do
        addChannelToBridge bridgeHandle connectedCalledChannel
        addChannelToBridge bridgeHandle connectedCallerChannel) >> timeCallback 3600000000 >> lift (dropChannel connectedCalledChannel) -- max call duration


data CallToDestResult = CallToDestTermNormally | CallToDestFailed
