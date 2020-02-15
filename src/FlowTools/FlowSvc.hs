{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module FlowTools.FlowSvc where

import Protolude

import Utils
import ARICore

import Control.Lens

class FlowSubSvc ev rq subs s | subs -> rq, subs -> ev where
  reqDisp :: Lens' subs (EventDistributor rq (FlowContextM s) s)
  evDistr :: Lens' subs (EventDistributor ev (FlowContextM s) s)

class GetSnapshot subs sshot | subs -> sshot where
  snapshot :: subs -> sshot

dispatchSvcRequest :: (FlowSubSvc ev rq subs s) => Lens' s subs -> rq -> FlowContext s ()
dispatchSvcRequest subSvc = notifyEvent (subSvc . reqDisp) -- rq

monitorSvcEvent :: (FlowSubSvc ev rq subs s) => Lens' s subs -> EventMonitor ev (FlowContextM s) -> FlowContext s EventID
monitorSvcEvent subSvc = addMonitor (subSvc . evDistr) -- evMon

monitorSvcEvent' :: (FlowSubSvc ev rq subs s) => Lens' s subs -> (ev -> ev') -> (ev' ->  FlowContext s ()) -> FlowContext s EventID
monitorSvcEvent' subSvc transEvFunc evHandler = monitorSvcEvent subSvc (EventMonitor (const True) (\_ ev -> evHandler (transEvFunc ev) >> return True))

-- monitorStateUpdate :: (HasSvcEvDistr s1 rs s) => Lens' s1 s -> EventMonitor rs (FlowContextM s1) -> FlowContext s1 EventID
-- monitorStateUpdate  subSvc = addMonitor (subSvc . svcEventDistr) -- evMon
