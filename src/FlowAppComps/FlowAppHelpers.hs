{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FlowAppComps.FlowAppHelpers
    ( dialWithPlanFlow
    , dialWithPlanCall
    , phoneNumFromDialConfig
    , callerIDFromDialConfig
    , DialConfig (..)
    , HasDialConfig (..)
    ) where

import Protolude
--import Data.Aeson
import Control.Lens
import Elm.Derive

import ARICore
import FlowTools

import FlowAppComps.DialingPlan

dialWithPlanFlow :: (HasPJSIPAccount s) => DialFunc (FlowContextM s) a -> Text -> Text ->FlowContext s a
dialWithPlanFlow  = dialWithPlan

dialWithPlanCall :: (HasPJSIPAccount s) => DialFunc (FlowCallbackM s) a -> Text -> Text ->FlowCallback s a
dialWithPlanCall  = dialWithPlan

data DialConfig = DialConfig
                { phoneNum :: Text
                , callerID :: Maybe Text
                , callerName :: Maybe Text
                , extraDialConfig :: Maybe [(Text,Text)]
                --, extraDialConfig :: Maybe ReservedData
                } deriving (Show, Generic)
--instance ToJSON DialConfig
--instance FromJSON DialConfig
deriveBoth defaultOptions ''DialConfig

class HasDialConfig s where
  dialConfig :: Lens' s (Maybe DialConfig)


phoneNumFromDialConfig :: (HasDialConfig s) => FlowContext s Text
phoneNumFromDialConfig = do
  dialCfgMaybe <- use dialConfig
  case dialCfgMaybe of
    Nothing -> throwError $ GenericError "No dial cfg is set"
    (Just dialCfg) -> return (phoneNum dialCfg)

callerIDFromDialConfig :: (HasDialConfig s) => Text -> FlowContext s Text
callerIDFromDialConfig defCallerID = do
  dialCfgMaybe <- use dialConfig
  return $ case dialCfgMaybe of
    Nothing -> defCallerID
    (Just dialCfg) -> case callerID dialCfg of
      Nothing -> defCallerID
      (Just cID) -> cID
