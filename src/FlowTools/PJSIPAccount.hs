{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FlowTools.PJSIPAccount
  ( HasPJSIPAccount (..)
  , PJSIPAccountData
  , resetPJSIPAccount
  , setPJSIPAccount
  , emptySIPAccount
  ) where

--import Prelude (String)import Control.Monad

import Protolude
import Control.Lens

import Utils
import ARICore

--loggerPath::String
--loggerPath=getLoggerPath "PJSIPAccount"

type PJSIPAccountData = Maybe (AccountConfig, PJSIPAccountID)
class HasPJSIPAccount s where
  pjSIPAccount :: Lens' s PJSIPAccountData

emptySIPAccount :: PJSIPAccountData
emptySIPAccount = Nothing

resetPJSIPAccount :: (HasPJSIPAccount s) => FlowContext s ()
resetPJSIPAccount = do
  pjSIPAccount .$ (`forM_` (\(_, accID)-> pjsipDeleteAccountCmd accID))
  pjSIPAccount .= Nothing

setPJSIPAccount :: (HasPJSIPAccount s) => AccountConfig -> FlowContext s ()
setPJSIPAccount accountConfig = do
  resetPJSIPAccount
  accountID <- pjsipCreateAccountCmd accountConfig
  pjSIPAccount .= Just (accountConfig , accountID)
