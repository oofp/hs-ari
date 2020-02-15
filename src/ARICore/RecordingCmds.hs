{-# LANGUAGE TupleSections #-}

module ARICore.RecordingCmds  (
        startRecordingCmd,
        pauseRecordingCmd,
        resumeRecordingCmd,
        muteRecordingCmd,
        unmuteRecordingCmd,
        stopStoreRecordingCmd,
        stopDiscardRecordingCmd,
        deleteStoredRecordingCmd,
        RecCapabale,
        RecordingParams (..),
        RecTerminateOn (..),
        RecordingFormat (..)
    ) where

import Prelude (String)
import Data.Text
import Control.Monad
import           qualified GHC.Show (Show (..))

--import Utils.LoggerSetup

import ARICore.HTTPCommand
import ARICore.ARIData
import ARICore.ARIFlow
import Protolude


-- loggerPath::String
-- loggerPath=getLoggerPath "RecordingCmds"

data RecordingFormat = RecFormatWav | RecFormatGsm
data RecTerminateOn = RecTermNone | RecTermAny | RecTermStar | RecTermPound | RecTerm1

instance Show RecordingFormat where
  show RecFormatGsm ="gsm"
  show RecFormatWav ="wav"

instance Show RecTerminateOn where
  show RecTermNone  = "none"
  show RecTermAny   = "any"
  show RecTermStar  = "*"
  show RecTermPound = "#"
  show RecTerm1     = "1"

data RecordingParams = RecordingParams
  {  recordFormat :: RecordingFormat      -- wav/gsm
  ,  maxDurationSeconds :: Int         -- Maximum duration of the recording, in seconds. 0 for no limit.
  ,  maxSilenceSeconds :: Int          -- Maximum duration of silence, in seconds. 0 for no limit.
  ,  beep  :: Bool                     -- Play beep when recording begins
  ,  terminateOn :: RecTerminateOn     --Allowed values: none, any, *, #
  }

class RecCapabale h where
  getRecCmdUrlPrefix :: h -> Text

instance RecCapabale ChannelHandle where
  getRecCmdUrlPrefix channelHandle  =  "channels/" <> getID channelHandle

instance RecCapabale BridgeHandle where
  getRecCmdUrlPrefix bridgeHandle  = "bridges/" <> getID bridgeHandle

startRecordingCmd :: (RecCapabale hnd) => RecordingParams -> ARIEventHandler s RecordingHandle -> hnd -> FlowContext  s (Maybe (RecordingHandle, Text))
startRecordingCmd recParams recEventHandler hnd = do
  hndAsync@(recHandle, _) <- createHandleAndMonitorFlow recEventHandler

  let urlPrefix = getRecCmdUrlPrefix hnd
      reqStr::String
      reqStr=unpack$ mconcat [urlPrefix, "/record?name=", getID recHandle,
        "&format=" , show (recordFormat recParams) ,
        "&maxDurationSeconds=" , show (maxDurationSeconds recParams) ,
        "&maxSilenceSeconds=" , show (maxSilenceSeconds recParams) ,
        "&beep=" , if beep recParams then "true" else "false",
        "&terminateOn=", show (terminateOn recParams)]
  catchError
      (postOp reqStr >> return (Just (recHandle, getID recHandle)))
      (\_e-> do
            releaseHandleFlow hndAsync
            return Nothing)

deleteStoredRecordingCmd :: Text->FlowContext s HTTPOK
deleteStoredRecordingCmd recordName=
  let reqStr::String
      reqStr=mconcat ["recordings/stored/", unpack recordName]
  in delOp reqStr

pauseRecordingCmd :: RecordingHandle->FlowContext s HTTPOK
pauseRecordingCmd recHandle=
  let reqStr::String
      reqStr=mconcat ["recordings/live/", (unpack.getID) recHandle, "/pause"]
  in postOp reqStr

resumeRecordingCmd :: RecordingHandle->FlowContext s HTTPOK
resumeRecordingCmd recHandle =
  let reqStr::String
      reqStr=mconcat ["recordings/live/", (unpack.getID) recHandle, "/pause"]
  in delOp reqStr

muteRecordingCmd :: RecordingHandle->FlowContext s HTTPOK
muteRecordingCmd recHandle =
  let reqStr::String
      reqStr=mconcat ["recordings/live/", (unpack.getID) recHandle, "/mute"]
  in postOp reqStr

unmuteRecordingCmd :: RecordingHandle->FlowContext s HTTPOK
unmuteRecordingCmd recHandle =
  let reqStr::String
      reqStr=mconcat ["recordings/live/", (unpack.getID) recHandle, "/mute"]
  in delOp reqStr

stopDiscardRecordingCmd :: RecordingHandle->FlowContext s HTTPOK
stopDiscardRecordingCmd recHandle =
  let reqStr::String
      reqStr=mconcat ["recordings/live/", (unpack.getID) recHandle]
  in delOp reqStr

stopStoreRecordingCmd :: RecordingHandle->FlowContext s HTTPOK
stopStoreRecordingCmd recHandle =
  let reqStr::String
      reqStr=mconcat ["recordings/live/", (unpack.getID) recHandle , "/stop"]
  in postOp reqStr
