{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TestTools.FlowCmds 
  ( Cmd (..)
  , toFlow
  ) where

import Protolude
import qualified Data.Aeson as DA
import Prelude (String)
--import FlowTools hiding (evDistr)
import ARIUtils
import ARICore
import Utils
import FlowTools.FlowCallback
import FlowTools.FlowCallbackComps
import FlowTools.FlowEvDistr
import FlowTools.TimerTool

data Cmd
  = Wait {waitMsec :: Integer}
  | SendDTMF {dtmfs :: String}
  | HangupCmd
  | CmdList { cmds :: [Cmd]}
  | PlayMusic
  | PlayRing
  | PlayBeep
  | PlayHello
  | PlaySoundFile {fileName :: Text}
  | WithTimeLimit {limitMsec :: Integer, cmd :: Cmd}
  | Repeat {repeatTimes :: Int, cmd :: Cmd}
  | Race {cmd1::Cmd, cmd2::Cmd}
  | Par {cmd1::Cmd, cmd2::Cmd}
  | RecordFile {maxRecTime :: Int, flBeep :: Bool}
  deriving (Show, Eq, Generic)

customOptions :: DA.Options
customOptions = DA.defaultOptions
  { DA.sumEncoding = DA.ObjectWithSingleField
  }

instance DA.ToJSON Cmd where
    toJSON     = DA.genericToJSON customOptions
    toEncoding = DA.genericToEncoding customOptions

instance DA.FromJSON Cmd where
    parseJSON     = DA.genericParseJSON customOptions

toFlow :: (HasARIEventDistributor s) => Cmd -> ConnectedChannel -> FlowCallback s ()
toFlow (Wait msec) _ = timeCallback (msec*1000)
toFlow (SendDTMF dtmfStr) conChan = lift $ sendDTMFChannel conChan dtmfStr
toFlow TestTools.FlowCmds.HangupCmd conChan = lift $ dropChannel conChan
toFlow (CmdList cmdLst) conChan =
  let
    cmdFuncList :: (HasARIEventDistributor s) => [ConnectedChannel -> FlowCallback s ()]
    cmdFuncList = toFlow <$> cmdLst
  in
    sequence_ (sequenceA cmdFuncList conChan)
toFlow (Repeat times cmdToRepeat) conChan = replicateM_ times (toFlow cmdToRepeat conChan)
toFlow (WithTimeLimit msec cmdLim) conChan=
  void $ toFlow (Wait msec) conChan >||< toFlow cmdLim conChan
toFlow (Race cmdFirst cmdSecond) conChan=
  void $ toFlow cmdFirst conChan >||< toFlow cmdSecond conChan
toFlow (Par cmdFirst cmdSecond) conChan=
  void $ bothCalls (toFlow cmdFirst conChan) (toFlow cmdSecond conChan)
toFlow PlayMusic conChan=
  void $ playMOHChannelComp conChan
toFlow PlayRing conChan=
  void $ playMOHChannelComp conChan
toFlow (PlaySoundFile soundFileName)  conChan =
  void $ playSoundOnChannelComp (PBSound (PBSoundParams soundFileName)) conChan
toFlow PlayBeep conChan = toFlow (PlaySoundFile "beep") conChan
toFlow PlayHello conChan = toFlow (PlaySoundFile "hello-world") conChan
toFlow (RecordFile recTime beepNeeded) (ChannelInState channelHnd) =
  let
    recParams =
      RecordingParams
        { recordFormat =  RecFormatGsm
        , maxDurationSeconds = recTime
        , maxSilenceSeconds = 0
        , beep = beepNeeded
        , terminateOn = RecTermNone
        }
  in do
    (_recHandle, _recID, recCallback) <- startRecording recParams channelHnd
    RecordingCompleted <- recCallback
    return ()


{-
toFlowTst :: Cmd -> ConnectedChannel -> FlowCallback s ()
toFlowTst (CmdList cmds) conChan =
  let
    cmdFuncList :: [ConnectedChannel -> FlowCallback s ()]
    cmdFuncList = toFlow <$> cmds
    chanFunc :: ConnectedChannel -> [FlowCallback s ()]
    chanFunc = sequenceA  cmdFuncList
  in
    sequence_ (chanFunc conChan)
-}

{-
cmdExample0 :: Cmd
cmdExample0 = CmdList
  [ Wait 1500
  , SendDTMF "2"
  , Wait 3000
  ]

cmdExample :: Cmd
cmdExample = CmdList
  [ Wait 1500
  , SendDTMF "2"
  , WithTimeLimit 2000 PlayRing
  , Repeat 3 PlayBeep
  , PlayHello
  , Repeat 2 (CmdList [ WithTimeLimit 1500 PlayRing, PlayHello])
  , PlaySoundFile "vm-goodbye"
  ]

storeCmd :: Cmd -> String -> IO ()
storeCmd cmdObj fileNm = Data.ByteString.Lazy.writeFile fileNm (DA.encode cmdObj)
-}
