{-# LANGUAGE RankNTypes #-}

module FlowTools.FlowEvDistr (
      createHandleWithEvents
    , createHandleWithEvents'
    , enableHanldeMonitoring
    , addARIEventMonitor
    , addARIEventMonitorWithTrans
    , addARIEventMonitor'
    , removeARIEventMonitor
    , transEvWithHandle
    , EventSource
    , ARISrcEvent (..)
    , ARIEventDistributor
    , ARIEventFilter
    , ARIEventMonitor
    , EventHandlerFunc
    , HasARIEventDistributor (..)
    , IsARIEventSource (..)
    ) where

--import Prelude (String)
import Protolude

--import Utils.LoggerSetup
import Utils
import ARICore

-- loggerPath::String
-- loggerPath=getLoggerPath "FlowEvDistr"

data EventSource = ChannelEventSource ChannelHandle | PlaybackEventSource PlaybackHandle | RecordingEventSource RecordingHandle deriving (Eq, Show)
data ARISrcEvent =  ARISrcEvent {_ariEvent :: ARIEvent, _eventSource :: EventSource} deriving (Eq, Show)
type ARIEventDistributor s = EventDistributor ARISrcEvent (FlowContextM s) s
type ARIEventDistributorLens s = EventDistributorLens ARISrcEvent (FlowContextM s) s
type ARIEventMonitor s = EventMonitor ARISrcEvent (FlowContextM s)
type EventHandlerFunc s ev = EventID -> ev -> FlowContext s Bool
type ARIEventFilter = ARISrcEvent -> Bool

class IsARIEventSource src where
  createSource:: src -> EventSource

instance IsARIEventSource ChannelHandle where
  createSource =  ChannelEventSource

instance IsARIEventSource PlaybackHandle where
  createSource =  PlaybackEventSource

instance IsARIEventSource RecordingHandle where
  createSource =  RecordingEventSource

class HasARIEventDistributor s where
  getEventDistributor :: (ARIEventDistributorLens s)

createHandleWithEvents' :: (IsARIEventSource h) => (ARIEventHandler s h -> FlowContext s (Maybe a)) -> ARIEventDistributorLens s -> FlowContext s (Maybe a)
createHandleWithEvents' handleFactory distr =
  handleFactory (\ev h -> notifyEvent distr (ARISrcEvent ev (createSource h)) >> continue)

createHandleWithEvents :: (IsARIEventSource h, HasARIEventDistributor s) => (ARIEventHandler s h -> FlowContext s (Maybe a)) -> FlowContext s (Maybe a)
createHandleWithEvents handleFactory = createHandleWithEvents' handleFactory getEventDistributor

enableHanldeMonitoring' :: (IsARIEventSource h, ARIHandle h) => h -> ARIEventDistributorLens s -> FlowContext s (Maybe (FlowContext s ()))
enableHanldeMonitoring' hnd distr = do
  asyncResMaybe <- createMonitorFlowForHanlde (\ev h -> notifyEvent distr (ARISrcEvent ev (createSource h)) >> continue) hnd
  return $ (liftIO . cancel) <$> asyncResMaybe

--TODO: fix typo  
enableHanldeMonitoring :: (IsARIEventSource h, ARIHandle h, HasARIEventDistributor s) => h -> FlowContext s (Maybe (FlowContext s ()))
enableHanldeMonitoring h = enableHanldeMonitoring'  h getEventDistributor

--createMonitorFlowForHanlde :: ARIHandle a => ARIEventHandler s a -> a -> FlowContext s (Maybe (Async ()))

addARIEventMonitor' :: ARIEventDistributorLens s -> ARIEventMonitor s -> FlowContext s EventID
addARIEventMonitor' = addMonitor

addARIEventMonitor ::  HasARIEventDistributor s => ARIEventMonitor s -> FlowContext s EventID
addARIEventMonitor = addMonitor getEventDistributor

addARIEventMonitorWithTrans ::  HasARIEventDistributor s => (ARISrcEvent -> Maybe ev') -> EventHandlerFunc s ev' -> FlowContext s EventID
addARIEventMonitorWithTrans = addMonitorWithTrans getEventDistributor

removeARIEventMonitor ::  HasARIEventDistributor s => EventID -> FlowContext s ()
removeARIEventMonitor = removeMonitor getEventDistributor

transEvWithHandle :: IsARIEventSource h => h ->(ARIEvent ->Maybe ev') -> (ARISrcEvent -> Maybe ev')
transEvWithHandle h transEv (ARISrcEvent ariEv src)
    | src == createSource h = transEv ariEv
    | otherwise = Nothing
