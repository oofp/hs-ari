{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module ARICore.PlaybackCmds
    (
        startPlaybackCmd,
        stopPlaybackCmd,
        controlPlaybackCmd,
        fileMedia,
        PBMedia (..),
        PBSoundParams (..),
        PBRecordingParams (..),
        PBCapabale,
        PBControlOp (..)
    ) where

import Protolude
import Data.Text
import Data.Aeson
import Data.String
import qualified GHC.Show (Show (..))

--mport Utils.LoggerSetup

import ARICore.HTTPCommand
import ARICore.ARIData
import ARICore.ARIFlow

newtype PBSoundParams     = PBSoundParams {soundName::Text} deriving Show
newtype PBRecordingParams = PBRecordingParams {recordingName::Text} deriving Show
data    PBMedia = PBSound PBSoundParams
                | PBRecording PBRecordingParams
                deriving Show

fileMedia :: Text -> PBMedia
fileMedia  = PBSound . PBSoundParams 

getSoundURI::PBMedia->Text
getSoundURI (PBSound (PBSoundParams sndName)) = "sound:" <> sndName
getSoundURI (PBRecording (PBRecordingParams recordName)) = "recording:" <> recordName


--loggerPath::String
--loggerPath=getLoggerPath "PlaybackCmds"

class PBCapabale h where
  getPBCmdUrlPrefix :: h -> Text

instance PBCapabale ChannelHandle where
  getPBCmdUrlPrefix channelHandle  =  "channels/" <> getID channelHandle

instance PBCapabale BridgeHandle where
  getPBCmdUrlPrefix bridgeHandle  = "bridges/" <> getID bridgeHandle

startPlaybackCmd :: (PBCapabale hnd) =>  PBMedia -> ARIEventHandler s PlaybackHandle -> hnd -> FlowContext s (Maybe PlaybackHandle)
startPlaybackCmd pbMedia pbEventHandler hnd = do
  hndAsync@(pbHandle, _) <- createHandleAndMonitorFlow pbEventHandler
  let urlPrefix = getPBCmdUrlPrefix hnd
      reqStr::String
      reqStr = unpack $ mconcat [urlPrefix, "/play/"::Text, getID pbHandle,"?media="::Text , getSoundURI pbMedia]
  catchError
      (postOp reqStr >> return (Just pbHandle))
      (\_e-> do
            releaseHandleFlow hndAsync
            return Nothing)

stopPlaybackCmd::PlaybackHandle->FlowContext s HTTPOK
stopPlaybackCmd pbHandle=
  let reqStr::String
      reqStr=mconcat ["playbacks/", (unpack.getID) pbHandle]
  in catchError (delOp reqStr) (\_ -> return (HTTPOK Nothing)) 

data PBControlOp= PBCtrlRestart | PBCtrlPause | PBCtrlUnpause |PBCtrlReverse | PBCtrlForward deriving (Eq, Generic)

instance Show PBControlOp where
  show PBCtrlRestart = "restart"
  show PBCtrlPause = "pause"
  show PBCtrlUnpause = "unpause"
  show PBCtrlReverse = "reverse"
  show PBCtrlForward = "forward"
instance ToJSON PBControlOp
instance FromJSON PBControlOp

controlPlaybackCmd::PlaybackHandle->PBControlOp->FlowContext s HTTPOK
controlPlaybackCmd pbHandle pbControlOperation=
  let reqStr::String
      reqStr=mconcat ["playbacks/", (unpack.getID) pbHandle,"/control?operation=",show pbControlOperation]
  in postOp reqStr
