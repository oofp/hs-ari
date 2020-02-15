{-# LANGUAGE DeriveGeneric #-}

module GroupCall.GroupCallReq where

import Protolude
import Data.Aeson
import ARICore

data DialMode = DialModeNothing | DialModeJoinPlain  | DialModeJoin  | DialModeBargeIn | DialModeHold | DialModeWhisper deriving (Show,Eq,Generic)
instance ToJSON DialMode
instance FromJSON DialMode

data DialReqParams            = DialReqParams               {dialClientID::Text, dialNumber::Text, dialName::Text, dialMode::DialMode} deriving (Show, Generic)
data AddToConfReqParams       = AddToConfReqParams          {addToConfCallID::Text} deriving (Show, Generic)
data RemoveFromConfReqParams  = RemoveFromConfReqParams     {removeFromConfCallID::Text} deriving (Show, Generic)
data AddToConfReqParams2      = AddToConfReqParams2         {addToConfCallID2::Text} deriving (Show, Generic)
data RemoveFromConfReqParams2 = RemoveFromConfReqParams2    {removeFromConfCallID2::Text} deriving (Show, Generic)
data DropCallReqParams        = DropCallReqParams           {dropCallID::Text} deriving (Show, Generic)
data HoldCallReqParams        = HoldCallReqParams           {holdCallID::Text} deriving (Show, Generic)
data UnholdCallReqParams      = UnholdCallReqParams         {unholdCallID::Text} deriving (Show, Generic)
data MuteCallReqParams        = MuteCallReqParams           {muteCallID::Text} deriving (Show, Generic)
data UnmuteCallReqParams      = UnmuteCallReqParams         {unmuteCallID::Text} deriving (Show, Generic)
data StartRingCallReqParams   = StartRingCallReqParams      {startRingCallID::Text} deriving (Show, Generic)
data StopRingCallReqParams    = StopRingCallReqParams       {stopRingCallID::Text} deriving (Show, Generic)
data PlayCallReqParams        = PlayCallReqParams           {playCallID::Text, sound::Text} deriving (Show, Generic)
data StartSnoopingReqParams   = StartSnoopingReqParams      {snoopCallID::Text} deriving (Show, Generic)
data StopSnoopingReqParams    = StopSnoopingReqParams       {unsnoopCallID::Text} deriving (Show, Generic)
data StartWhisperingReqParams = StartWhisperingReqParams    {whisperCallID::Text} deriving (Show, Generic)
data StopWhisperingReqParams  = StopWhisperingReqParams     {unwhisperCallID::Text} deriving (Show, Generic)
data StartRecordingReqParams  = StartRecordingReqParams     {recClientReqID::Text} deriving (Show, Generic)
data StopRecordingReqParams   = StopRecordingReqParams      {stopRecID::Text, store::Bool} deriving (Show, Generic)
data MuteRecordingReqParams   = MuteRecordingReqParams      {muteRecID::Text} deriving (Show, Generic)
data UnmuteRecordingReqParams = UnmuteRecordingReqParams    {unmuteRecID::Text} deriving (Show, Generic)
data PauseRecordingReqParams  = PauseRecordingReqParams     {pauseRecID::Text} deriving (Show, Generic)
data ResumeRecordingReqParams = ResumeRecordingReqParams    {resumeRecID::Text} deriving (Show, Generic)
data StartRecPlayingReqParams = StartRecPlayingReqParams    {playClientReqID::Text, recordingID::Text} deriving (Show, Generic)
data StopRecPlayingReqParams  = StopRecPlayingReqParams     {stopPlayID::Text} deriving (Show, Generic)
data ControlRecPlayingReqParams  = ControlRecPlayingReqParams {ctrlPlayID::Text, pbControl::PBControlOp} deriving (Show, Generic)

instance ToJSON DialReqParams
instance FromJSON DialReqParams
instance ToJSON RemoveFromConfReqParams
instance FromJSON RemoveFromConfReqParams
instance ToJSON RemoveFromConfReqParams2
instance FromJSON RemoveFromConfReqParams2
instance ToJSON AddToConfReqParams
instance FromJSON AddToConfReqParams
instance ToJSON AddToConfReqParams2
instance FromJSON AddToConfReqParams2
instance ToJSON DropCallReqParams
instance FromJSON DropCallReqParams
instance ToJSON HoldCallReqParams
instance FromJSON HoldCallReqParams
instance ToJSON UnholdCallReqParams
instance FromJSON UnholdCallReqParams
instance ToJSON MuteCallReqParams
instance FromJSON MuteCallReqParams
instance ToJSON UnmuteCallReqParams
instance FromJSON UnmuteCallReqParams
instance ToJSON StartRingCallReqParams
instance FromJSON StartRingCallReqParams
instance ToJSON StopRingCallReqParams
instance FromJSON StopRingCallReqParams
instance ToJSON PlayCallReqParams
instance FromJSON PlayCallReqParams
instance ToJSON StartSnoopingReqParams
instance FromJSON StartSnoopingReqParams
instance ToJSON StopSnoopingReqParams
instance FromJSON StopSnoopingReqParams
instance ToJSON StartWhisperingReqParams
instance FromJSON StartWhisperingReqParams
instance ToJSON StopWhisperingReqParams
instance FromJSON StopWhisperingReqParams
instance FromJSON StartRecordingReqParams
instance FromJSON StopRecordingReqParams
instance FromJSON MuteRecordingReqParams
instance FromJSON UnmuteRecordingReqParams
instance FromJSON PauseRecordingReqParams
instance FromJSON ResumeRecordingReqParams
instance ToJSON StartRecordingReqParams
instance ToJSON StopRecordingReqParams
instance ToJSON MuteRecordingReqParams
instance ToJSON UnmuteRecordingReqParams
instance ToJSON PauseRecordingReqParams
instance ToJSON ResumeRecordingReqParams
instance FromJSON StartRecPlayingReqParams
instance FromJSON StopRecPlayingReqParams
instance FromJSON ControlRecPlayingReqParams
instance ToJSON StartRecPlayingReqParams
instance ToJSON StopRecPlayingReqParams
instance ToJSON ControlRecPlayingReqParams

data CallReqParams = CallReqParams
  {
    callClientID::Text,
    callNumber::Text,
    callName::Text
  } deriving (Show, Generic)

data CallbackRequestParams = CallbackRequestParams
  { phoneParams :: CallReqParams
  , destinations :: [CallReqParams]
  } deriving (Show, Generic)

instance ToJSON CallReqParams
instance FromJSON CallReqParams
instance ToJSON CallbackRequestParams
instance FromJSON CallbackRequestParams

data GroupCallReq =  DialReq              DialReqParams
                  |  AddToConf            AddToConfReqParams
                  |  AddToConf2           AddToConfReqParams2
                  |  RemoveFromConf       RemoveFromConfReqParams
                  |  RemoveFromConf2      RemoveFromConfReqParams2
                  |  DropCall             DropCallReqParams
                  |  HoldCall             HoldCallReqParams
                  |  UnholdCall           UnholdCallReqParams
                  |  MuteCall             MuteCallReqParams
                  |  UnmuteCall           UnmuteCallReqParams
                  |  StartRing            StartRingCallReqParams
                  |  StopRing             StopRingCallReqParams
                  |  PlayCall             PlayCallReqParams
                  |  DropAll
                  |  StartSnoopCall       StartSnoopingReqParams
                  |  StopSnoopCall        StopSnoopingReqParams
                  |  StartWhisperingCall  StartWhisperingReqParams
                  |  StopWhisperingCall   StopWhisperingReqParams
                  |  StartRecording       StartRecordingReqParams
                  |  StopRecording        StopRecordingReqParams
                  |  MuteRecording        MuteRecordingReqParams
                  |  UnmuteRecording      UnmuteRecordingReqParams
                  |  PauseRecording       PauseRecordingReqParams
                  |  ResumeRecording      ResumeRecordingReqParams
                  |  StartRecPlaying      StartRecPlayingReqParams
                  |  StopRecPlaying       StopRecPlayingReqParams
                  |  ControlRecPlaying    ControlRecPlayingReqParams
                  |  GetSnapshot
                  |  CallbackRequest      CallbackRequestParams
                  |  CallbackParRequest   CallbackRequestParams
                  deriving (Show,Generic)

instance ToJSON GroupCallReq
instance FromJSON GroupCallReq
