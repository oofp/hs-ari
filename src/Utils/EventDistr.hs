{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Utils.EventDistr
    (
      newEventDistributor,
      addMonitor,
      addMonitorWithTrans,
      removeMonitor,
      notifyEvent,
      monitors,
      EventDistributor,
      EventDistributorLens,
      EventMonitor (..),
      EventID
    ) where


import Protolude
import qualified Protolude as P
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Control.Lens

type EventID = Int
type EventHandler ev m = EventID -> ev -> m Bool

data EventMonitor ev m= EventMonitor {filter :: ev->Bool, handler :: EventHandler ev m}
type EventDistributorLens ev m s = Lens' s (EventDistributor ev m s)
data EventDistributor ev m s = EventDistributor {_lastIndex :: EventID, _handlers :: Map EventID (EventMonitor ev m)}

makeLenses ''EventDistributor
--instance Functor

newEventDistributor :: EventDistributor ev m s
newEventDistributor = EventDistributor 0 M.empty

monitors :: (MonadState s) m => EventDistributorLens ev m s -> m [EventID]
monitors this = keys <$> use (this.handlers)

addMonitor :: (MonadState s) m => EventDistributorLens ev m s -> EventMonitor ev m -> m EventID
addMonitor this evMon =
  this %%=
    (\(EventDistributor lIndex hnds) ->
      let newLastIndex=lIndex+1
          newHandlers=insert lIndex evMon hnds
      in (lIndex, EventDistributor newLastIndex newHandlers))

removeMonitor :: (MonadState s) m => EventDistributorLens ev m s -> EventID -> m ()
removeMonitor this evID =
  this %=
    (\(EventDistributor lstIndex hnds) ->
      let !newHandlers=delete evID hnds
      in EventDistributor lstIndex newHandlers)


notifyEvent :: (MonadState s) m => EventDistributorLens ev m s -> ev -> m ()
notifyEvent this ev = void $ notifyEvent' this ev

notifyEvent' :: (MonadState s) m => EventDistributorLens ev m s -> ev -> m Int
notifyEvent' this ev = do
  st <- P.get
  let (EventDistributor _ hnds) = view this st
      hndsToNotify = M.filter (\(EventMonitor fltr _ ) -> fltr ev) hnds
      hndsList = M.toList hndsToNotify
      len = length hndsList
  forM_ hndsList (\(evID, EventMonitor _ hndFunc) -> (hndFunc evID ev >>= (\flContinue -> unless flContinue (removeMonitor this evID))))
  return  $! len

evHandlerWithTrans :: (MonadState s) m => (ev->Maybe ev') -> EventHandler ev' m -> EventHandler ev m
evHandlerWithTrans transFunc evHandler' evID ev = case transFunc ev of
    Nothing ->    return True
    (Just ev') -> evHandler' evID ev'

addMonitorWithTrans :: (MonadState s) m => EventDistributorLens ev m s -> (ev->Maybe ev') -> EventHandler ev' m -> m EventID
addMonitorWithTrans this transFunc evHandler' =
  let evMonitor = EventMonitor (const True) (evHandlerWithTrans transFunc evHandler')
  in addMonitor this evMonitor
