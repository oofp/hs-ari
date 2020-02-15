module ARICore.ChannelHelper (
      whenDisconnected
    , whenConnected
    , whenConnectedEv
    , whenRinging
    , whenRingingEv
    , ChannelEventHandler
    ) where

import Prelude (String)
import Protolude

import Utils.LoggerSetup

import ARICore.ARIData
import ARICore.ARIHelper
import ARICore.ARIFlow
import ARICore.ARIChannel

_loggerPath::String
_loggerPath=getLoggerPath "ChannelHelper"

isChannelStateChangedTo :: ChannelState -> ARIEvent -> Bool
isChannelStateChangedTo stateToCheck (ChannelEvent (ChannelEventData ChannelStateChange ch)) = ARICore.ARIChannel.state ch == stateToCheck
isChannelStateChangedTo _ _ = False

type ChannelEventHandler s = ARIEventHandler s ChannelHandle

whenRingingEv  :: ChannelEventHandler s -> ChannelEventHandler s -> ChannelEventHandler s
whenRingingEv = whenEvent  (isChannelStateChangedTo Ringing)
whenRinging  :: FlowHandler s -> ChannelEventHandler s -> ChannelEventHandler s
whenRinging =  whenEvent'' whenRingingEv


whenConnectedEv  :: ChannelEventHandler s -> ChannelEventHandler s -> ChannelEventHandler s
whenConnectedEv = whenEvent (isChannelStateChangedTo Up)
whenConnected  :: FlowHandler s -> ChannelEventHandler s-> ChannelEventHandler s
whenConnected =  whenEvent'' whenConnectedEv

whenDisconnectedEv  :: ChannelEventHandler s -> ChannelEventHandler s-> ChannelEventHandler s
whenDisconnectedEv = whenEvent (\ev -> isChannelStasisEnd ev || isChannelDestroyed ev)
whenDisconnected  :: FlowHandler s -> ChannelEventHandler s-> ChannelEventHandler s
whenDisconnected = whenEvent'' whenDisconnectedEv

{-
testfn = whenRingingEv  noHandling $
         whenConnectedEv noHandling noHandling
testfn' = whenRinging  continue $
          whenConnected continue noHandling

-}
