{-# LANGUAGE RankNTypes #-}

module ARICore.ARIHelper (
      whenEvent
    , whenEvent'
    , whenEvent''
    , handlingDone
    , continue
    , stop
    , exitFlow
    , submitTask
    , EventPredicate
    ) where

import Protolude
import Control.Concurrent.STM.TChan

import ARICore.ARIData
import ARICore.ARIFlow

-- loggerPath::String
-- loggerPath=getLoggerPath "ARIHelper"

type EventPredicate=ARIEvent->Bool

ehFromFH :: FlowHandler s -> ARIEventHandler s h
ehFromFH hnd _ _ =  hnd

whenEvent :: EventPredicate -> ARIEventHandler s h -> ARIEventHandler s h -> ARIEvent -> h -> FlowHandler s
whenEvent evPred evHandler  nextHandler ev h
  | evPred ev = evHandler ev h
  | otherwise = nextHandler ev h

whenEvent' :: (EventPredicate -> ARIEventHandler s h -> ARIEventHandler s h -> ARIEvent -> h -> FlowHandler s) ->(EventPredicate -> FlowHandler s -> ARIEventHandler s h -> ARIEvent -> h -> FlowHandler s)
whenEvent' f  = \evPred flowHandler -> f evPred (ehFromFH flowHandler)

whenEvent'' :: (ARIEventHandler s h -> ARIEventHandler s h -> ARIEvent -> h -> FlowHandler s) ->(FlowHandler s -> ARIEventHandler s h -> ARIEvent -> h -> FlowHandler s)
whenEvent'' f  = \flowHandler -> f (ehFromFH flowHandler)

continue :: FlowHandler s
continue = return True

stop :: FlowHandler s
stop = return False

handlingDone :: ARIEventHandler s h
handlingDone _ _ = continue

exitFlow :: FlowContext s ()
exitFlow = do
  notificChannel <- getNotifChannel
  liftIO.atomically $ writeTChan notificChannel stop

submitTask :: FlowContext s () -> FlowContext s ()
submitTask task = do
  notificChannel <- getNotifChannel
  liftIO.atomically $ writeTChan notificChannel (task >> return True)

{-
data EventSource = ChannelEventSource ChannelHandle | PlaybackEventSource PlaybackHandle | RecordingEventSource RecordingHandle deriving (Eq, Show)
data ARISrcEvent =  ARISrcEvent {_ariEvent :: ARIEvent, _eventSource :: EventSource} deriving (Eq, Show)
type ARIEventDistributorLens s = EventDistributorLens ARISrcEvent (FlowContextM s) s

class IsARIEventSource src where
  createSource:: src -> EventSource

instance IsARIEventSource ChannelHandle where
  createSource =  ChannelEventSource

class HasARIEventDistributor s where
  getEventDistributor :: (ARIEventDistributorLens s)

createHandleWithEvents' :: (IsARIEventSource h) => (ARIEventHandler s h -> FlowContext s (Maybe h)) -> ARIEventDistributorLens s -> FlowContext s (Maybe h)
createHandleWithEvents' handleFactory distr =
  handleFactory (\ev h -> notifyEvent distr (ARISrcEvent ev (createSource h)) >> continue)

createHandleWithEvents :: (IsARIEventSource h, HasARIEventDistributor s) => (ARIEventHandler s h -> FlowContext s (Maybe h)) -> FlowContext s (Maybe h)
createHandleWithEvents handleFactory = createHandleWithEvents' handleFactory getEventDistributor
-}
