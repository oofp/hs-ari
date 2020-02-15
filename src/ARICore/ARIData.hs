{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ARICore.ARIData
  where

--import qualified Data.ByteString as BStr
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Protolude
import           Data.Map.Strict
import           ARICore.ARIChannel
import           Data.Aeson.Types (Parser)
import           Utils

data PlaybackState = Queued
                   | Playing
                   | Completed
                   deriving (Show, Eq)

instance FromJSON PlaybackState where
  parseJSON (String "queued")   =  pure  Queued
  parseJSON (String "playing")  =  pure  Playing
  parseJSON (String "done") =      pure  Completed

  parseJSON _          = mempty

data Playback = Playback
              { playbackID::        Text
              , playbackMediaUri::  Text
              , playbackTargetUri:: Text
              , playbackLanguage::  Text
              , playbackState::     PlaybackState
              } deriving (Show, Eq)
instance FromJSON Playback where
  parseJSON (Object v)   =  Playback <$> v .: "id"
                                     <*> v .: "media_uri"
                                     <*> v .: "target_uri"
                                     <*> v .: "language"
                                     <*> v .: "state"
  parseJSON _ = mempty


-- channelEventTypeString::[Text]
-- channelEventTypeString=fmap show ((enumFromTo minBound maxBound)::[ChannelEventType])

data ChannelEventType = StasisStart
                      | StasisEnd
                      | ChannelCreated
                      | ChannelDestroyed
                      | ChannelHangupRequest
                      | ChannelStateChange
                      | ChannelTalkingStarted
                      | ChannelTalkingFinished
                      deriving (Eq, Show, Read, Enum, Bounded)

data ChannelEventData = ChannelEventData
    { channelEventType :: ChannelEventType
    , channel          :: Channel
    } deriving (Eq, Show)

data DTMFEventData = DTMFEventData
    { dtmfChannel      :: Channel
    , digit            :: DTMFDigit
    , duration         :: Int
    } deriving (Eq, Show)

data PlaybackEventType = PlaybackStarted
                       | PlaybackFinished
                       deriving (Eq, Show, Enum, Bounded)

data PlaybackEventData = PlaybackEventData
    { playbackEventType :: PlaybackEventType
    , playback          :: Playback
    } deriving (Eq, Show)

data RecordingState = RecQueued
                    | RecRecording
                    | RecPaused
                    | RecDone
                    | RecFailed
                    | RecCanceled
                    deriving (Eq, Show)

instance FromJSON RecordingState where
  parseJSON (String "queued")   =  pure  RecQueued
  parseJSON (String "recording")=  pure  RecRecording
  parseJSON (String "paused")   =  pure  RecPaused
  parseJSON (String "done")     =  pure  RecDone
  parseJSON (String "failed")   =  pure  RecFailed
  parseJSON (String "canceled") =  pure  RecCanceled

  parseJSON _          = mempty

data LiveRecording = LiveRecording
              { recCause::              Maybe Text
              , recDuration::           Maybe Int
              , recFormat::             Maybe Text
              , recName::               Text
              , recState::              RecordingState
              , recTargetUri::          Text
              , recSilenceDuration::    Maybe Int
              , recTalkingDuration::    Maybe Int
              } deriving (Show, Eq)

instance FromJSON LiveRecording where
  parseJSON (Object v)   =  LiveRecording
                                     <$> v .:? "cause"
                                     <*> v .:? "duration"
                                     <*> v .:? "format"
                                     <*> v .:  "name"
                                     <*> v .:  "state"
                                     <*> v .:  "target_uri"
                                     <*> v .:? "silence_duration"
                                     <*> v .:? "talking_duration"
  parseJSON _ = mempty


data RecordingEventType = RecordingStarted
                        | RecordingFinished
                        | RecordingFailed
                        deriving (Eq, Show, Enum, Bounded)

data RecordingEventData = RecordingEventData
    { recordingEventType  :: RecordingEventType
    , recording           :: LiveRecording
    } deriving (Eq, Show)


data ARIEvent = ChannelEvent    ChannelEventData
              | DTMFEvent       DTMFEventData
              | PlaybackEvent   PlaybackEventData
              | RecordingEvent  RecordingEventData
              | BridgeEvent
              deriving (Eq, Show)

data ARIEventKeyType = CH | PB | REC | BRD deriving (Eq, Show, Ord)
data ARIEventKey = ARIKey ARIEventKeyType Text deriving (Eq, Show, Ord)

instance Hashable ARIEventKey where
  hashWithSalt salt (ARIKey _ textKey)=hashWithSalt salt textKey

class ARIHandle a where
  getID  :: a -> Text
  fromID :: Text -> a
  eventKey::a-> ARIEventKey

newtype ChannelHandle = ChannelHandle {channelID::Text} deriving (Eq, Show)
instance ARIHandle ChannelHandle where
  getID = channelID
  fromID = ChannelHandle
  eventKey h = ARIKey CH (channelID h)

newtype PlaybackHandle = PlaybackHandle {pbID::Text} deriving (Eq, Show)
instance ARIHandle PlaybackHandle where
  getID = pbID
  fromID = PlaybackHandle
  eventKey h = ARIKey PB (pbID h)

newtype RecordingHandle = RecordingHandle {recordingID::Text} deriving (Eq, Show)
instance ARIHandle RecordingHandle where
  getID = recordingID
  fromID = RecordingHandle
  eventKey h = ARIKey REC (recordingID h)

newtype BridgeHandle = BridgeHandle {bridgeID::Text} deriving (Eq, Show)
instance ARIHandle BridgeHandle where
  getID = bridgeID
  fromID = BridgeHandle
  eventKey h = ARIKey BRD (bridgeID h)

data ARIResource =  ChannelResource   ChannelHandle
                 |  PlaybackResource  PlaybackHandle
                 |  RecordingResource RecordingHandle

getARIEventKey::ARIEvent->Maybe ARIEventKey
getARIEventKey (ChannelEvent (ChannelEventData _ ch))   =  Just $ ARIKey CH (getChannelID ch)
getARIEventKey (DTMFEvent (DTMFEventData ch _ _))       =  Just $ ARIKey CH (getChannelID ch)
getARIEventKey (PlaybackEvent (PlaybackEventData _ pb)) =  Just $ ARIKey PB (playbackID pb)
getARIEventKey (RecordingEvent (RecordingEventData _ rec)) =  Just $ ARIKey REC (recName rec)
getARIEventKey BridgeEvent                              =  Nothing

isFinalEvent::ARIEvent->Bool
isFinalEvent (ChannelEvent (ChannelEventData StasisEnd _ ))             = True
isFinalEvent (ChannelEvent (ChannelEventData ChannelDestroyed _))       = True
isFinalEvent (PlaybackEvent (PlaybackEventData PlaybackFinished _ ))    = True
isFinalEvent (RecordingEvent (RecordingEventData RecordingFinished _ )) = True
isFinalEvent (RecordingEvent (RecordingEventData RecordingFailed _ ))   = True
isFinalEvent _                                                          = False

type ChannelStateTxt=Text

isChannelState::[ChannelState]->ARIEvent->Bool
isChannelState states (ChannelEvent (ChannelEventData ChannelStateChange ch))  = ARICore.ARIChannel.state ch `elem` states
isChannelState _  _                                                            = False

isChannelUpStateChange::ARIEvent->Bool
isChannelUpStateChange (ChannelEvent (ChannelEventData ChannelStateChange ch))  = ARICore.ARIChannel.state ch == Up
isChannelUpStateChange _ = False

isChannelStasisEnd::ARIEvent->Bool
isChannelStasisEnd (ChannelEvent (ChannelEventData StasisEnd _))  = True
isChannelStasisEnd _ = False

isDTMFReceived::ARIEvent->Bool
isDTMFReceived (DTMFEvent _) = True
isDTMFReceived _ = False

digitReceivedFromEvent::ARIEvent->Maybe DTMFDigit
digitReceivedFromEvent (DTMFEvent dtmfEvData) = Just $ digit dtmfEvData
digitReceivedFromEvent _ = Nothing

isChannelDestroyed::ARIEvent->Bool
isChannelDestroyed (ChannelEvent (ChannelEventData ChannelDestroyed _))  = True
isChannelDestroyed _ = False

getChannelInitEvent::ARIEvent->Maybe (ChannelEventData, ChannelHandle)
getChannelInitEvent (ChannelEvent chEventData@(ChannelEventData StasisStart ch))  =  Just (chEventData, fromID (getChannelID ch))
getChannelInitEvent _                                                             =  Nothing

parseChannelEvent::ChannelEventType->Object->Parser ARIEvent
parseChannelEvent chEventType v =
    let chEventData=ChannelEventData chEventType <$> v .: "channel"
    in  ChannelEvent <$> chEventData

--channelEventParsers=fmap (\evType->(show evType::Text, parseChannelEvent evType)) (enumFromTo minBound maxBound::[ChannelEventType])

getEventParsers::(Enum a, Bounded a, Show a)=>(a->(Object->Parser ARIEvent))->[(Text,Object->Parser ARIEvent)]
getEventParsers parseEvent=fmap (\evType->(show evType::Text, parseEvent evType)) (enumFromTo minBound maxBound)

channelEventParsers::[(Text,Object->Parser ARIEvent)]
channelEventParsers=getEventParsers parseChannelEvent

parseDtmfEvent::Object->Parser ARIEvent
parseDtmfEvent v =
  let dtmfEventData=DTMFEventData <$> v .: "channel" <*> v .: "digit" <*> v .: "duration_ms"
  in  DTMFEvent <$> dtmfEventData

parsePlaybackEvent::PlaybackEventType->Object->Parser ARIEvent
parsePlaybackEvent pbEventType v =
    let pbEventData=PlaybackEventData pbEventType <$> v .: "playback"
    in  PlaybackEvent <$> pbEventData

parseRecordingEvent::RecordingEventType->Object->Parser ARIEvent
parseRecordingEvent recEventType v =
    let recEventData=RecordingEventData recEventType <$> v .: "recording"
    in  RecordingEvent <$> recEventData

pbEventParsers::[(Text,Object->Parser ARIEvent)]
pbEventParsers=getEventParsers parsePlaybackEvent

recEventParsers::[(Text,Object->Parser ARIEvent)]
recEventParsers=getEventParsers parseRecordingEvent

eventParsers::[(Text,Object->Parser ARIEvent)]
eventParsers = mconcat [channelEventParsers, pbEventParsers, recEventParsers, [("ChannelDtmfReceived",parseDtmfEvent)]]

ariEventParsers::Map Text (Object -> Parser ARIEvent)
ariEventParsers=fromList eventParsers


instance FromJSON ARIEvent where
    parseJSON val@(Object v) = case val ^? key "type" . _String of
        Just eventTypeText -> case lookup eventTypeText ariEventParsers of
          Just parser ->      parser v
          _           ->      mempty
        _                 -> mempty
    parseJSON _ = mempty
