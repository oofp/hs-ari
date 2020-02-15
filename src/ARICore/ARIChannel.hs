{-# LANGUAGE DeriveGeneric     #-}

module ARICore.ARIChannel where

--import qualified Data.ByteString as BStr
import qualified ARICore.CallerID             as CID
import           Data.Aeson
import           Protolude hiding (Down)

data DialplanCEP = DialplanCEP
                { context::Text
                , exten::Text
                , priority::Int
                } deriving (Show,  Eq, Generic)

instance FromJSON DialplanCEP

data ChannelState = Down
                  | Rsrved
                  | OffHook
                  | Dialing
                  | Ring
                  | Ringing
                  | Up
                  | Busy
                  | DialingOffhook
                  | Prering
                  | Unknown deriving (Eq, Show)


fromText :: Text -> ChannelState
fromText "Down"           = Down
fromText "Rsrved"         = Rsrved
fromText "OffHook"        = OffHook
fromText "Dialing"        = Dialing
fromText "Ring"           = Ring
fromText "Ringing"        = Ringing
fromText "Up"             = Up
fromText "Busy"           = Busy
fromText "Dialing Offhook"= DialingOffhook
fromText "Pre-ring"       = Prering
fromText "Unknown"        = Unknown
fromText _                = Unknown

instance FromJSON ChannelState where
  parseJSON (String txt)   =  pure $  fromText txt
  parseJSON _          =      mempty

getChannelID  ::Channel->Text
getChannelID  = ARICore.ARIChannel.id

data Channel = Channel
             { id::Text
             , name::Text
             , state::ChannelState
             , caller::CID.CallerID
             , connected::CID.CallerID
             , accountcode::Text
             , dialplan::DialplanCEP
             , creationtime::Text
             , language::Text} deriving (Generic, Show, Eq)

instance FromJSON Channel
