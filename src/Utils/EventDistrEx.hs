{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Utils.EventDistrEx
    (
      newEventDistributor,
      addMonitor,
      addMonitorWithTrans,
      removeMonitor,
      notifyEvent,
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

data EventMonitor ev m d = EventMonitor {filter :: ev->Bool, handler :: EventHandler ev m, monData :: d}
type EventDistributorLens ev m s d = Lens' s (EventDistributor ev m s d)
data EventDistributor ev m s d = EventDistributor {_lastIndex :: EventID, _handlers :: Map EventID (EventMonitor ev m d)}

--makeLenses ''EventDistributor
--instance Functor

newEventDistributor :: EventDistributor ev m s d
newEventDistributor = EventDistributor 0 M.empty

addMonitor :: (MonadState s) m => EventDistributorLens ev m s d -> EventMonitor ev m d -> m EventID
addMonitor this evMon =
  this %%=
    (\(EventDistributor lIndex hnds) ->
      let newLastIndex=lIndex+1
          newHandlers=insert lIndex evMon hnds
      in (newLastIndex, EventDistributor newLastIndex newHandlers))

removeMonitor :: (MonadState s) m => EventDistributorLens ev m s d -> EventID -> m ()
removeMonitor this evID =
  this %=
    (\(EventDistributor lstIndex hnds) ->
      let newHandlers=delete evID hnds
      in EventDistributor lstIndex newHandlers)


notifyEvent :: (MonadState s) m => EventDistributorLens ev m s d -> ev -> m ()
notifyEvent this ev = do
  st <- P.get
  let (EventDistributor _ hnds) = view this st
      hndsToNotify = M.filter (\(EventMonitor fltr _ _) -> fltr ev) hnds
  forM_ (M.toList hndsToNotify) (\(evID, EventMonitor _ hndFunc _) -> (hndFunc evID ev >>= (\flContinue -> unless flContinue (removeMonitor this evID))))

evHandlerWithTrans :: (MonadState s) m => (ev->Maybe ev') -> EventHandler ev' m -> EventHandler ev m
evHandlerWithTrans transFunc evHandler' evID ev = case transFunc ev of
    Nothing ->    return True
    (Just ev') -> evHandler' evID ev'


addMonitorWithTrans :: (MonadState s) m => EventDistributorLens ev m s d -> (ev->Maybe ev') -> EventHandler ev' m -> d -> m EventID
addMonitorWithTrans this transFunc evHandler' mData =
  let evMonitor = EventMonitor (const True) (evHandlerWithTrans transFunc evHandler') mData
  in addMonitor this evMonitor
