{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GroupCall.GroupCallData where

import Protolude
import Data.Aeson
import Control.Lens

newtype CallID      = CallID Text deriving (Show,Eq,Ord)
newtype RecorderID  = RecordingID Text deriving (Show,Eq,Ord)
newtype PlayerID    = PlayerID Text deriving (Show,Eq,Ord)
newtype BridgeID    = BridgeID Text deriving (Show,Eq,Ord)

data GroupCallConfig = GroupCallConfig
  { _callerID :: Text
  , _provider :: Text
  } deriving Show

makeLenses ''GroupCallConfig

data GroupCallStatus
  = NoCall
  | Dialing
  | Ringing
  | Connected
  | Offered
  | Accepted
  | Disconnected
  deriving (Show, Eq, Generic)

instance ToJSON GroupCallStatus
instance FromJSON GroupCallStatus

data ConferCallStatus
  = NoConference
  | Conferenced
  | Whispering
  deriving (Show, Eq, Generic)

instance ToJSON ConferCallStatus
instance FromJSON ConferCallStatus

data GroupCall = GroupCall
  { gcID :: Text
  , clientID :: Text
  , gcContactName :: Text
  , gcContactNumber :: Text
  , gcStatus :: GroupCallStatus
  , bridgeID :: Maybe Text
  , snoopBridgeID :: Maybe Text
  , onHold :: Bool
  , muted :: Bool
  , talking :: Bool
  , gcConfStatus :: ConferCallStatus --can be derived
  , playing :: Bool
  , ringing :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON GroupCall
instance FromJSON GroupCall

initGroupCall::Text->Text->Text->Text->GroupCall
initGroupCall callID clnID contactName contactNumber=
  GroupCall callID clnID contactName contactNumber NoCall Nothing Nothing False False False NoConference False False

data CallRecordingStatus = RecordingInitiated | RecordingStarted | RecordingFinished | RecordingFailed deriving (Show, Eq, Generic)
instance ToJSON CallRecordingStatus
instance FromJSON CallRecordingStatus

data CallRecording = CallRecording
  { recID :: Text
  , recClientID ::Text
  , recStatus :: CallRecordingStatus
  , recPaused :: Bool
  , recMuted :: Bool
  , recStored :: Bool
  } deriving (Show, Eq, Generic)
instance ToJSON CallRecording
instance FromJSON CallRecording

data PlayStatus = PlayInitiated | PlayStarted | PlayActive | PlayFinished deriving (Show, Eq, Generic)
instance ToJSON PlayStatus
instance FromJSON PlayStatus

data PlayingState = PlayingState
  { playID :: Text
  , playClientID :: Text
  , playStatus :: PlayStatus
  , playPaused :: Bool
  } deriving (Show, Eq, Generic)
instance ToJSON PlayingState
instance FromJSON PlayingState

data ServerResponse = ServerResponse {servRespID :: Text} deriving (Show, Eq, Generic)
instance ToJSON ServerResponse
instance FromJSON ServerResponse

data GroupCallEvent = GroupCallChangeEvent GroupCall | BridgeRecordingChangeEvent CallRecording | PlayingStateChangeEvent PlayingState | ServerResponseEvent ServerResponse deriving (Show, Eq, Generic)
instance ToJSON GroupCallEvent
instance FromJSON GroupCallEvent
