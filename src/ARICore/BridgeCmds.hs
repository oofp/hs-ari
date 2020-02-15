{-# LANGUAGE TupleSections #-}

module ARICore.BridgeCmds
    (
        createBridgeCmd,
        deleteBridgeCmd,
        addChannelToBridgeCmd,
        removeChannelFromBridgeCmd
    ) where

import Protolude
import Prelude (String)
import Data.Text

import ARICore.HTTPCommand
import ARICore.ARIData
import ARICore.ARIFlow

-- loggerPath::String
-- loggerPath=getLoggerPath "BridgeCmds"

--newBridgeHM :: FlowContext s BridgeHM
--newBridgeHM = createHandleAndMonitorFlow

newBridgeHandle :: FlowContext s BridgeHandle
newBridgeHandle = createHandleFlow

createBridgeCmd :: FlowContext s BridgeHandle
createBridgeCmd = do
  hnd <- newBridgeHandle

  --http://localhost:8088/ari/bridges/bridge100"
  let reqStr::String
      reqStr=mconcat ["bridges/", (unpack.getID) hnd]
  _ <- postOp reqStr

  return hnd

-- "http://localhost:8088/ari/bridges/bridge100/addChannel?channel=channel200"
addChannelToBridgeCmd :: BridgeHandle->ChannelHandle->FlowContext s HTTPOK
addChannelToBridgeCmd bridgeHandle channelHandle=
  let reqStr::String
      reqStr=mconcat ["bridges/", (unpack.getID) bridgeHandle , "/addChannel?channel=", (unpack.getID) channelHandle]
  in postOp reqStr

-- "http://localhost:8088/ari/bridges/bridge100/removeChannel?channel=channel100"
removeChannelFromBridgeCmd :: BridgeHandle -> ChannelHandle -> FlowContext s HTTPOK
removeChannelFromBridgeCmd bridgeHandle channelHandle=
  let reqStr::String
      reqStr=mconcat ["bridges/", (unpack.getID) bridgeHandle , "/removeChannel?channel=", (unpack.getID) channelHandle]
  in postOp reqStr

-- "http://localhost:8088/ari/bridges/bridge100"
deleteBridgeCmd::BridgeHandle->FlowContext s ()
deleteBridgeCmd bridgeHandle=
  let reqStr::String
      reqStr=mconcat ["bridges/", (unpack.getID) bridgeHandle]
  in delOp reqStr >> releaseHandleOnlyFlow bridgeHandle
