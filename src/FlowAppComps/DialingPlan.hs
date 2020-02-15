{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FlowAppComps.DialingPlan
  ( DialPlanRes (..)
  , HasDialPlan (..)
  , DialFunc
  , dialWithPlan
  ) where

import Protolude
import Control.Lens

import ARICore
import FlowTools

data DialPlanRes = DialPlanRes {_tech :: Tech, _provider :: Provider, _destParty :: Party}
makeLenses ''DialPlanRes

--class HasDialPlan s where
--  dialPlan :: Party -> FlowContext s (Maybe DialPlanRes)

class Monad m => HasDialPlan (m :: * -> *) where
  dialPlan :: Party -> m (Maybe DialPlanRes)

--pjSIPDialPlan :: HasPJSIPAccount s => Party -> FlowContext s (Maybe DialPlanRes)
--pjSIPDialPlan party = do
--  pjSIPMaybe <- use pjSIPAccount
--  return $ fmap (\(_,accID)-> DialPlanRes PJSIP (Provider accID) party) pjSIPMaybe

pjSIPDialPlan :: (MonadState s m, HasPJSIPAccount s) => Party -> m (Maybe DialPlanRes)
pjSIPDialPlan party = do
  pjSIPMaybe <- use pjSIPAccount
  return $ fmap (\(_,accID)-> DialPlanRes PJSIP (Provider accID) party) pjSIPMaybe

instance HasPJSIPAccount s => HasDialPlan (FlowContextM s) where
  dialPlan = pjSIPDialPlan

instance HasPJSIPAccount s => HasDialPlan (FlowCallbackM s) where
  dialPlan = pjSIPDialPlan

--instance (MonadState s m , HasPJSIPAccount s) => HasDialPlan (m s) where
--  dialPlan = pjSIPDialPlan'

--instance HasPJSIPAccount s => HasDialPlan s where
--   dialPlan party = pjSIPDialPlan party
----

type DialFunc m a  = Tech -> Provider -> Party -> Party -> m a

--dialWithPlan :: HasPJSIPAccount s => (Tech -> Provider -> Party -> Party -> FlowContext s a) -> Text -> Text -> FlowContext s a
dialWithPlan :: (HasDialPlan m, MonadError FlowError m)  => DialFunc m a -> Text -> Text -> m a
dialWithPlan dialFunc callerID dest = do
  let dstParty = Party dest
  dpResMaybe <- dialPlan dstParty
  case dpResMaybe of
    Nothing -> throwError $ GenericError "no dialplan"
    (Just (DialPlanRes dialTech prov destParty')) -> dialFunc dialTech prov (Party callerID) destParty'
