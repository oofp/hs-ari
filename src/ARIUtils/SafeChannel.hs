{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module ARIUtils.SafeChannel where

import Protolude hiding (Down)
import Prelude (String)
import ARICore

data ChannelInState (st::ChannelState) = ChannelInState ChannelHandle
type RingingChannel = ChannelInState 'Ringing
type ConnectedChannel = ChannelInState 'Up
type InitiatedChannel = ChannelInState 'Down


addChannelToBridge :: BridgeHandle -> ConnectedChannel -> FlowContext s ()
addChannelToBridge bridgeHandle (ChannelInState channelHnd) = void $ addChannelToBridgeCmd bridgeHandle channelHnd

removeChannelFromBridge :: BridgeHandle -> ConnectedChannel -> FlowContext s ()
removeChannelFromBridge bridgeHandle (ChannelInState channelHnd) = void $ unfail $ removeChannelFromBridgeCmd bridgeHandle channelHnd

startRingChannel :: ConnectedChannel -> FlowContext s ()
startRingChannel (ChannelInState channelHnd) = void $ startRingChannelCmd channelHnd

stopRingChannel :: ConnectedChannel -> FlowContext s ()
stopRingChannel (ChannelInState channelHnd) = void $ stopRingChannelCmd channelHnd

startMOHChannel :: ConnectedChannel -> FlowContext s ()
startMOHChannel (ChannelInState channelHnd) = void $ startMOHChannelCmd channelHnd

stopMOHChannel :: ConnectedChannel -> FlowContext s ()
stopMOHChannel (ChannelInState channelHnd) = void $ stopMOHChannelCmd channelHnd

sendDTMFChannel :: ConnectedChannel -> String -> FlowContext s ()
sendDTMFChannel (ChannelInState channelHnd) dtmfStr = void $ sendDTMFChannelCmd channelHnd dtmfStr

dropChannel :: ChannelInState st -> FlowContext s ()
dropChannel (ChannelInState channelHnd) = void $ unfail $ dropChannelCmd channelHnd

startMuteChannel :: MuteDirection -> ConnectedChannel -> FlowContext s ()
startMuteChannel muteDir (ChannelInState channelHnd) = void $ startMuteChannelCmd muteDir channelHnd

stopMuteChannel ::  MuteDirection -> ConnectedChannel -> FlowContext s ()
stopMuteChannel muteDir (ChannelInState channelHnd) = void $ stopMuteChannelCmd muteDir channelHnd

--TODO: trust app here with  ChannelHandle
answerChannel ::  ChannelHandle  -> FlowContext s ConnectedChannel
answerChannel channelHnd = answerChannelCmd channelHnd >> return (ChannelInState channelHnd)

{-
getRingingChannel :: ChannelHandle -> RingingChannel
getRingingChannel h  = ChannelInState h

fromHandle :: ChannelHandle -> ChannelInState st
fromHandle = ChannelInState

-}
