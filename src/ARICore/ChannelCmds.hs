{-# LANGUAGE TupleSections #-}

module ARICore.ChannelCmds
    (
      dialOutCmd,
      dropChannelCmd,
      startRingChannelCmd,
      stopRingChannelCmd,
      startMuteChannelCmd,
      stopMuteChannelCmd,
      startMOHChannelCmd,
      stopMOHChannelCmd,
      sendDTMFChannelCmd,  
      startRingingChannelCmd,
      stopRingingChannelCmd,
      createSnoopChannelCmd,
      enableChannelTalkDetectCmd,
      enableChannelDenoiseCmd,
      answerChannelCmd,
      Tech (..),
      Party (..),
      Provider (..),
      MuteDirection (..),
      SpyDir (..),
      WhisperDir (..)
    ) where

import Prelude (String)
import Data.Text
import GHC.Show
import qualified Protolude as P
import Protolude

import Utils.LoggerSetup

import ARICore.HTTPCommand
import ARICore.ARIData
import ARICore.ARIFlow
import ARICore.ARIHelper
import Data.Time.Clock.POSIX

loggerPath::String
loggerPath=getLoggerPath "ChannelCmds"

data Tech = SIP | PJSIP deriving (Eq, Show)

newtype Party = Party Text deriving (Eq, Show)
newtype Provider = Provider Text deriving (Eq, Show)

dropChannelCmd :: ChannelHandle->FlowContext s HTTPOK
dropChannelCmd channelHandle  =
  let reqStr::String
      reqStr="channels/"<>(unpack.getID) channelHandle
  in do
    curTime <- liftIO tmcsec -- $ getPOSIXTime  
    httpRes <- delOp reqStr
    newTime <- liftIO  tmcsec -- getPOSIXTime  
    let diffTime = newTime - curTime
    myid <- liftIO myThreadId
    liftIO $ debugM loggerPath ("dropChannelCmd tID:" <> P.show myid <> " took:" <> P.show diffTime)
    return httpRes

createChannelWithReq :: (ChannelHandle -> FlowContext s HTTPOK) -> ARIEventHandler s ChannelHandle -> FlowContext s (Maybe ChannelHandle)
createChannelWithReq req evHandler = do
    hndAsync@(channelHandle, _) <- createHandleAndMonitorFlow evHandler
    catchError
              (req channelHandle >> return (Just channelHandle))
              (\_e-> do
                    releaseHandleFlow hndAsync
                    return Nothing)

tmcsec :: IO Integer
tmcsec = round . (1000000 *) <$> getPOSIXTime                    
dialOutCmd :: Tech -> Provider -> Party -> Party -> ARIEventHandler s ChannelHandle -> FlowContext s (Maybe ChannelHandle)
dialOutCmd tech provider callerID destParty  = createChannelWithReq (dialReqCmd tech destParty provider callerID) --evHandler
  where
    dialReqCmd :: Tech->Party->Provider->Party->ChannelHandle->FlowContext s HTTPOK
    dialReqCmd techn (Party dest) (Provider prv) (Party origCallerID) channelHandle = do
      appName <- getApp
      let reqStr::String
          reqStr=mconcat ["channels/", (unpack.getID) channelHandle, "?endpoint=", P.show techn, "/",unpack dest,"@",unpack prv,"&app=",appName,"&callerId=",unpack origCallerID]
      liftIO $ debugM loggerPath ("dialReq:" <> reqStr)
      curTime <- liftIO tmcsec -- $ getPOSIXTime  
      httpRes <- postOp reqStr
      newTime <- liftIO  tmcsec -- getPOSIXTime  
      let diffTime = newTime - curTime
      myid <- liftIO myThreadId
      liftIO $ debugM loggerPath ("dialOutCmd from:" <> P.show callerID <> " to:" <> P.show destParty <> " tID:" <> P.show myid <> " took:" <> P.show diffTime)
      return httpRes

data SpyDir = SpyNone | SpyIn | SpyOut | SpyBoth deriving (Eq)
data WhisperDir = WhisperNone | WhisperIn | WhisperOut | WhisperBoth deriving (Eq)

instance Show SpyDir where
  show SpyNone  = "none"
  show SpyIn    = "in"
  show SpyOut   = "out"
  show SpyBoth  = "both"

instance Show WhisperDir where
  show WhisperNone  = "none"
  show WhisperIn    = "in"
  show WhisperOut   = "out"
  show WhisperBoth  = "both"

createSnoopChannelCmd :: SpyDir -> WhisperDir -> ChannelHandle -> ARIEventHandler s ChannelHandle -> FlowContext s (Maybe ChannelHandle)
createSnoopChannelCmd spyDir whisperDir channelHandle evHandler= do
    appName <- getApp
    createChannelWithReq (snoopReq appName) evHandler
  where
    snoopReq appName snoopHandle=
      let reqStr::String
          reqStr="channels/" <> (unpack.getID) channelHandle <> "/snoop/" <> (unpack.getID) snoopHandle <> "?spy=" <> P.show spyDir <> "&whisper=" <> P.show whisperDir <> "&app=" <> appName
      in postOp reqStr

startRingChannelCmd :: ChannelHandle->FlowContext s HTTPOK
startRingChannelCmd channelHandle =
  let reqStr::String
      reqStr="channels/" <> (unpack.getID) channelHandle <> "/ring"
  in postOp reqStr

stopRingChannelCmd :: ChannelHandle->FlowContext s HTTPOK
stopRingChannelCmd channelHandle =
  let reqStr::String
      reqStr="channels/" <> (unpack.getID) channelHandle <> "/ring"
  in delOp reqStr

data MuteDirection =MuteDirBoth | MuteDirIn | MuteDirOut deriving (Show, Eq)

muteChannelUrl::MuteDirection->ChannelHandle->String
muteChannelUrl muteDir channelHandle = "channels/" <> (unpack.getID) channelHandle <> "/mute?direction=" <> muteStr muteDir
  where
    muteStr MuteDirBoth = "both"
    muteStr MuteDirIn   = "in"
    muteStr MuteDirOut  = "out"

startMuteChannelCmd :: MuteDirection->ChannelHandle->FlowContext s HTTPOK
startMuteChannelCmd muteDir channelHandle =
  let reqStr=muteChannelUrl muteDir channelHandle
  in postOp reqStr

stopMuteChannelCmd :: MuteDirection->ChannelHandle->FlowContext s HTTPOK
stopMuteChannelCmd muteDir channelHandle =
  let reqStr=muteChannelUrl muteDir channelHandle
  in delOp reqStr

mohChannelUrl::ChannelHandle->String
mohChannelUrl channelHandle = "channels/" <> (unpack.getID) channelHandle <> "/moh"

startMOHChannelCmd :: ChannelHandle->FlowContext s HTTPOK
startMOHChannelCmd channelHandle =
  let reqStr=mohChannelUrl channelHandle
  in postOp reqStr

stopMOHChannelCmd :: ChannelHandle->FlowContext s HTTPOK
stopMOHChannelCmd channelHandle =
  let reqStr=mohChannelUrl channelHandle
  in delOp reqStr

sendDTMFChannelCmd :: ChannelHandle -> String -> FlowContext s HTTPOK
sendDTMFChannelCmd channelHandle dtmfStr =
  let reqStr = "channels/" <> (unpack.getID) channelHandle <> "/dtmf?dtmf=" <> dtmfStr
  in postOp reqStr

ringChannelUrl::ChannelHandle->String
ringChannelUrl channelHandle = "channels/" <> (unpack.getID) channelHandle <> "/ring"

startRingingChannelCmd :: ChannelHandle->FlowContext s HTTPOK
startRingingChannelCmd channelHandle =
  let reqStr=ringChannelUrl channelHandle
  in postOp reqStr

stopRingingChannelCmd :: ChannelHandle->FlowContext s HTTPOK
stopRingingChannelCmd channelHandle =
  let reqStr=ringChannelUrl channelHandle
  in delOp reqStr

answerChannelCmd :: ChannelHandle->FlowContext s HTTPOK
answerChannelCmd channelHandle =
  let reqStr= "channels/" <> (unpack.getID) channelHandle <> "/answer"
  in postOp reqStr
  
-- curl -v -u boris:boris -X POST "http://localhost:8088/ari/channels/1468119167.42/variable?variable=TALK_DETECT(set)&value=1000,1500"
setChannelVarUrl::ChannelHandle->String->String->String
setChannelVarUrl channelHandle var val = "channels/" <> (unpack.getID) channelHandle <> "/variable?variable=" <> var <> "(set)&value" <> val

enableChannelTalkDetectCmd :: ChannelHandle->FlowContext s HTTPOK
enableChannelTalkDetectCmd channelHandle =
  let reqStr=setChannelVarUrl channelHandle "TALK_DETECT" "500,800"
  in postOp reqStr

enableChannelDenoiseCmd :: ChannelHandle->FlowContext s HTTPOK
enableChannelDenoiseCmd channelHandle =
  let reqStr=setChannelVarUrl channelHandle "DENOISE" "800,1500"
  in postOp reqStr


-- TODO :: needed ?
_channelUpOrDownPred::EventPredicate
_channelUpOrDownPred ev = isChannelUpStateChange ev || isChannelStasisEnd ev
