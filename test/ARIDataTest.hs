{-# LANGUAGE QuasiQuotes       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module ARIDataTest (ariDataTest) where

--import qualified Data.ByteString as BStr
import           ARICore
import qualified Data.ByteString.Lazy as LBStr
import           Text.RawString.QQ
import           Protolude hiding (state)
import           Data.Aeson
import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception (evaluate)
import           Control.Monad


upChannelStr::LBStr.ByteString
upChannelStr = [r|{
"id": "1467734013.14",
"name": "SIP/zoipper-00000006",
"state": "Up",
"caller": {
  "name": "",
  "number": "zoipper"
},
"connected": {
  "name": "",
  "number": ""
},
"accountcode": "",
"dialplan": {
  "context": "from-sip",
  "exten": "1000",
  "priority": 3
},
"creationtime": "2016-07-05T11:53:33.133-0400",
"language": "en"
}
|]

decodeUpChannel::Maybe Channel
decodeUpChannel=decode  upChannelStr

channelHangupRequestStr::LBStr.ByteString
channelHangupRequestStr = [r|
{
  "cause": 127,
  "type": "ChannelHangupRequest",
  "timestamp": "2016-07-05T21:23:15.016-0400",
  "channel": {
    "id": "1467734013.14",
    "name": "SIP/zoipper-00000006",
    "state": "Up",
    "caller": {
      "name": "",
      "number": "zoipper"
    },
    "connected": {
      "name": "",
      "number": ""
    },
    "accountcode": "",
    "dialplan": {
      "context": "from-sip",
      "exten": "1000",
      "priority": 3
    },
    "creationtime": "2016-07-05T11:53:33.133-0400",
    "language": "en"
  },
  "application": "hello-world"
}
|]

decodeChannelHangupRequest::Maybe ARIEvent
decodeChannelHangupRequest=decode channelHangupRequestStr

decodeChannelStateChangeEvent::Maybe ARIEvent
decodeChannelStateChangeEvent=decode channelStateChangeStr

decodeChannelStateChangeUpEvent::Maybe ARIEvent
decodeChannelStateChangeUpEvent=decode channelStateChangeUpStr

decodeDtmfReceivedEvent::Maybe ARIEvent
decodeDtmfReceivedEvent=decode channelDtmfReceivedStr

decodePB::Maybe Playback
decodePB=decode  pbStr

decodePBStartedEvent::Maybe ARIEvent
decodePBStartedEvent=decode pbStartedStr

pbStr::LBStr.ByteString
pbStr = [r|{
 "id": "playb100",
 "media_uri": "recording:record000",
 "target_uri": "channel:channel200",
 "language": "en",
 "state": "playing"
}
|]

pbStartedStr::LBStr.ByteString
pbStartedStr = [r|{
  "type": "PlaybackStarted",
  "playback": {
    "id": "playb100",
    "media_uri": "recording:record000",
    "target_uri": "channel:channel200",
    "language": "en",
    "state": "playing"
  },
  "application": "hello-world"
}
|]

channelStateChangeStr::LBStr.ByteString
channelStateChangeStr = [r|
{
  "type": "ChannelStateChange",
  "timestamp": "2016-07-09T11:48:56.361-0400",
  "channel": {
    "id": "channel200",
    "name": "SIP/zoipper-00000000",
    "state": "Ringing",
    "caller": {
      "name": "",
      "number": "18001234567"
    },
    "connected": {
      "name": "",
      "number": "18001234567"
    },
    "accountcode": "",
    "dialplan": {
      "context": "from-sip",
      "exten": "",
      "priority": 1
    },
    "creationtime": "2016-07-09T11:48:56.032-0400",
    "language": "en"
  },
  "application": "hello-world"
}
|]

channelStateChangeUpStr::LBStr.ByteString
channelStateChangeUpStr = [r|
{
  "type": "ChannelStateChange",
  "timestamp": "2016-07-09T11:49:01.308-0400",
  "channel": {
    "id": "channel200",
    "name": "SIP/zoipper-00000000",
    "state": "Up",
    "caller": {
      "name": "",
      "number": "18001234567"
    },
    "connected": {
      "name": "",
      "number": "18001234567"
    },
    "accountcode": "",
    "dialplan": {
      "context": "from-sip",
      "exten": "",
      "priority": 1
    },
    "creationtime": "2016-07-09T11:48:56.032-0400",
    "language": "en"
  },
  "application": "hello-world"
}
|]

channelDtmfReceivedStr::LBStr.ByteString
channelDtmfReceivedStr = [r|
{
  "type": "ChannelDtmfReceived",
  "timestamp": "2016-07-09T11:54:01.134-0400",
  "digit": "1",
  "duration_ms": 120,
  "channel": {
    "id": "channel200",
    "name": "SIP/zoipper-00000000",
    "state": "Up",
    "caller": {
      "name": "",
      "number": "18001234567"
    },
    "connected": {
      "name": "",
      "number": "18001234567"
    },
    "accountcode": "",
    "dialplan": {
      "context": "from-sip",
      "exten": "",
      "priority": 1
    },
    "creationtime": "2016-07-09T11:48:56.032-0400",
    "language": "en"
  },
  "application": "hello-world"
}
|]

-- (decode $ LC8.pack channelHangupRequestStr)::Maybe ARIEvent
-- let readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Just x; _ -> Nothing
-- Text.Read
recStartedStr::LBStr.ByteString
recStartedStr="{\n  \"type\": \"RecordingStarted\",\n  \"recording\": {\n    \"name\": \"0e06e21d-9bce-49db-b4c6-502344eb109f\",\n    \"format\": \"wav\",\n    \"state\": \"recording\",\n    \"target_uri\": \"bridge:ecb81a7a-2c72-43e0-bee9-1c5668913729\"\n  },\n  \"application\": \"hello-world\"\n}"
decodeRectartedEvent::Maybe ARIEvent
decodeRectartedEvent=decode recStartedStr

recStr::LBStr.ByteString
recStr="{\n    \"name\": \"0e06e21d-9bce-49db-b4c6-502344eb109f\",\n    \"format\": \"wav\",\n    \"state\": \"recording\",\n    \"target_uri\": \"bridge:ecb81a7a-2c72-43e0-bee9-1c5668913729\"\n  }"
recStr1::LBStr.ByteString
recStr1="{\n    \"cause\": \"state100\", \"name\": \"0e06e21d-9bce-49db-b4c6-502344eb109f\",\n    \"format\": \"wav\",\n    \"state\": \"recording\",\n    \"target_uri\": \"bridge:ecb81a7a-2c72-43e0-bee9-1c5668913729\"\n  }"
decodeRec::Maybe LiveRecording
decodeRec=decode recStr
decodeRec1::Maybe LiveRecording
decodeRec1=decode recStr1

decodeDtmfReceived::Maybe ARIEvent
decodeDtmfReceived = decode channelDtmfReceivedStr

ariDataTest :: IO ()
ariDataTest = hspec $ do
  describe "ARIData Test" $ do
    it "decode ChannelStateChange Up event" $
      fmap state decodeUpChannel `shouldBe` Just Up
    it "decode Recording started event" $
      decodeRectartedEvent `shouldNotBe` Nothing
    it "decode LiveRecording1" $
      decodeRec1 `shouldNotBe` Nothing
    it "decode LiveRecording" $
      decodeRec `shouldNotBe` Nothing
    it "decode dtmfDetected" $ --do
      --print decodeDtmfReceived
      decodeDtmfReceived `shouldNotBe` Nothing

