module ARIUtils.ChannelUtils (
      newDialer
    , dialAndPlay
    ) where

import Protolude

import ARICore

--loggerPath::String
--loggerPath=getLoggerPath "ChannelUtils"

type Dialer s= Text -> ChannelEventHandler s-> FlowContext s (Maybe ChannelHandle)
newDialer :: Tech -> Provider -> Party -> Dialer s
newDialer tech prov callerID destText = dialOutCmd tech prov callerID (Party destText)

dialAndPlay :: Dialer s -> Text ->  PBMedia -> FlowContext s (Maybe ChannelHandle)
dialAndPlay dialer dest pbMedia =
  dialer dest $
    whenConnectedEv (\_ev channelHnd -> startPlaybackCmd pbMedia handlingDone channelHnd >> continue) handlingDone
