{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ARICore.CallerID where

--import qualified Data.ByteString as BStr
import Protolude
import Data.Aeson

data CallerID = CallerID {name::Text,number::Text} deriving (Generic, Show, Eq)
instance FromJSON CallerID
