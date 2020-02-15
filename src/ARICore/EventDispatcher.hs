{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module ARICore.EventDispatcher
    (
      EventDisp,
      EventMonitor (..),
      ChannelsMap,
      EventDispData,
      NewChannelHandler,
      EventPlacement (..),
      ARIEventEntry,
      createEventDispatcher,
      newMonitorForHandle,
      createHandle,
      createHandleAndMonitor,
      createChannelMap,
      cloneMonitor,
      cloneMonitorSTM,
      releaseHandle,
    ) where

import qualified StmContainers.Map as STMMap
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Reader as R
import Protolude
import qualified Network.WebSockets  as WS
import qualified Data.ByteString.Lazy as LBStr
import Data.Aeson
import Data.Text
import qualified Data.UUID.V4 as UUIDGen
import Prelude (String)

import Utils.LoggerSetup
import ARICore.ARIData

data EventPlacement = InitEvent | InterimEvent | FinalEvent deriving (Show, Eq)
type ARIEventEntry = (ARIEvent, EventPlacement)
type EventChannel=TChan ARIEventEntry
type ChannelsMap=STMMap.Map ARIEventKey EventChannel
newtype EventMonitor = EventMonitor {evChannel::EventChannel}
type NewChannelHandler = EventDispData -> ChannelEventData -> ChannelHandle -> EventMonitor ->IO ()
type EventDispData = ChannelsMap
type EventDisp a = ReaderT EventDispData IO a

loggerPath::String
loggerPath=getLoggerPath "EventDispatcher"

createChannelMap::IO ChannelsMap
createChannelMap = STMMap.newIO

getChannelMap::EventDisp ChannelsMap
getChannelMap = R.ask

getEventChannelSTM::ARIEventKey->ChannelsMap->STM (Maybe EventChannel)
getEventChannelSTM =  STMMap.lookup

getEventChannel::ARIEventKey->EventDisp (Maybe EventChannel)
getEventChannel key= do
  channelsMap <- getChannelMap
  let channelMaybeSTM= STMMap.lookup key channelsMap
  liftIO $ atomically channelMaybeSTM

insertEventChannelSTM::ARIEventKey->ChannelsMap->STM EventChannel
insertEventChannelSTM key channelsMap=do
    newChannel <- newBroadcastTChan
    STMMap.insert newChannel key channelsMap
    return newChannel

insertEventChannelWithDupSTM::ARIEventKey->ChannelsMap->STM (EventChannel,EventChannel)
insertEventChannelWithDupSTM key channelsMap=do
    newChannel <- insertEventChannelSTM key channelsMap
    dupChannel <- dupTChan newChannel
    return (newChannel,dupChannel)


getOrInsertEventChannelSTM::ARIEventKey->ChannelsMap->STM (EventChannel, Maybe EventChannel)
getOrInsertEventChannelSTM key channelsMap= do
    channelMaybe <- STMMap.lookup key channelsMap
    case  channelMaybe of
      (Just ch) -> return (ch, Nothing) -- channel exsted
      Nothing -> (\(f,s)->(f,Just s)) <$> insertEventChannelWithDupSTM key channelsMap

getAndRemoveEventChannelSTM::ARIEventKey->ChannelsMap->STM (Maybe EventChannel)
getAndRemoveEventChannelSTM key channelsMap= do
     channelMaybe <- STMMap.lookup key channelsMap
     when  (isJust channelMaybe) (STMMap.delete key channelsMap)
     return channelMaybe

getAndRemoveEventChannel::ARIEventKey->EventDisp (Maybe EventChannel)
getAndRemoveEventChannel key = do
  channelsMap <- getChannelMap
  let channelSTM = getAndRemoveEventChannelSTM key channelsMap
  liftIO $ atomically channelSTM

getOrInsertEventChannel::ARIEventKey->EventDisp (EventChannel, Maybe EventChannel)
getOrInsertEventChannel key= do
  channelsMap <- getChannelMap
  let channelSTM=getOrInsertEventChannelSTM key channelsMap
  liftIO $ atomically channelSTM

removeEventChannel::ARIEventKey->EventDisp ()
removeEventChannel key= do
  channelsMap <- getChannelMap
  liftIO $ atomically $ STMMap.delete key channelsMap

releaseHandle :: ARIHandle a => a -> EventDisp ()
releaseHandle ariHande  = removeEventChannel (eventKey ariHande)

decodeEventAndKey::LBStr.ByteString->Either Text (ARIEventKey,ARIEvent)
decodeEventAndKey bytes =
  let ariEventMaybe::Maybe ARIEvent
      ariEventMaybe = decode bytes
  in case ariEventMaybe of
    Nothing -> Left "Failed to decode "
    (Just ariEvent) -> case getARIEventKey ariEvent of
      Nothing -> Left "Failed to get key "
      (Just ariEventKey) -> Right (ariEventKey, ariEvent)

createEventDispatcher:: WS.Connection-> NewChannelHandler -> EventDisp ()
createEventDispatcher conn newChannelHandler = loop where
    loop = do
      msgRcvd <- liftIO $ WS.receiveData conn
      liftIO $ debugM loggerPath ("Message received:"++show msgRcvd)
      let evAndKey=decodeEventAndKey msgRcvd
      case evAndKey of
        (Left err)        -> liftIO $ debugM loggerPath  ( unpack err <> show msgRcvd)
        (Right (key, ev)) -> dispatchARIEvent key ev newChannelHandler
      loop 

dispatchARIEvent::ARIEventKey->ARIEvent-> NewChannelHandler -> EventDisp ()
dispatchARIEvent key ariEvent newChannelHandler = do
    channelMaybe <- getChannelForEventL key ariEvent newChannelHandler
    case channelMaybe of
        Nothing    -> return () -- do nothing
        (Just (ch,plc))  -> liftIO $ atomically $ writeTChan ch (ariEvent,plc)

getChannelForEventL::ARIEventKey->ARIEvent-> NewChannelHandler -> EventDisp (Maybe (EventChannel, EventPlacement))
getChannelForEventL key event newChannelHandler = do
    eventChannelMaybe <- getChannelForEvent key event newChannelHandler
    case eventChannelMaybe of
      Just _  -> liftIO $ debugM loggerPath ("Channel found for event:" <> show event)
      _       -> liftIO $ debugM loggerPath ("No channel found for:" <> show event)
    return eventChannelMaybe

getChannelForEvent::ARIEventKey -> ARIEvent-> NewChannelHandler -> EventDisp (Maybe (EventChannel, EventPlacement))
getChannelForEvent key event newChannelHandler =
    case getChannelInitEvent event of
      Just (channelEventData, channelHandle)  -> getChannelForInitEvent channelEventData channelHandle 
      Nothing                                 -> getChannelForEvent'
    where
      getChannelForEvent'
        | isFinalEvent event  = setEventPlacement FinalEvent (getAndRemoveEventChannel key)
        | otherwise           = setEventPlacement InterimEvent (getEventChannel key)
      setEventPlacement plc evChWrapped = do
        evChMaybe <- evChWrapped
        return $ fmap (,plc) evChMaybe
      getChannelForInitEvent channelEventData channelHandle =
        do
          (evChan, monChanMaybe) <- getOrInsertEventChannel key
          _ <- sequenceA $ fmap notifyNewChannelHandler monChanMaybe --change Maybe EventDisp to EventDisp Maybe
          return $ Just (evChan , InitEvent)
        where
          notifyNewChannelHandler::EventChannel -> EventDisp ()
          notifyNewChannelHandler monChan = do
            liftIO $ debugM loggerPath ("notifyNewChannelHandler entered")
            eventDispData <- R.ask
            _ <- liftIO $ async $ newChannelHandler eventDispData channelEventData channelHandle  (EventMonitor monChan)
            return ()

getMonitor :: -- forall a.
               ARIHandle a =>
               a -> ChannelsMap -> IO (Maybe EventMonitor)
getMonitor ariHande channelsMap = atomically $ do
  evChannelMaybe <- getEventChannelSTM (eventKey ariHande) channelsMap
  let  chanSTM::STM (Maybe EventChannel)
       chanSTM=sequenceA $ fmap dupTChan evChannelMaybe
  fmap (fmap EventMonitor) chanSTM

newMonitorForHandle :: ARIHandle a => a -> EventDisp (Maybe EventMonitor)
newMonitorForHandle ariHandle  = do
  channelsMap <- getChannelMap
  liftIO $ getMonitor ariHandle channelsMap

createHandleIO :: ARIHandle a => ChannelsMap -> IO a
createHandleIO channelsMap = do
  uuid <- UUIDGen.nextRandom
  let resID = show uuid
      hnd = fromID resID
      key = eventKey hnd
  _ <- atomically $ insertEventChannelSTM key channelsMap
  return hnd

--createHandleAndMonitorIO :: forall a. ARIHandle a => ChannelsMap -> IO (a, EventMonitor)
createHandleAndMonitorIO :: ARIHandle a => ChannelsMap -> IO (a, EventMonitor)
createHandleAndMonitorIO  channelsMap = do
    hnd <- createHandleIO channelsMap
    (Just mon) <- getMonitor hnd channelsMap
    return (hnd,mon)

--createHandle :: forall a. ARIHandle a => EventDisp a
createHandle :: ARIHandle a => EventDisp a
createHandle =
  getChannelMap >>= (liftIO.createHandleIO)

createHandleAndMonitor :: ARIHandle a => EventDisp (a, EventMonitor)
createHandleAndMonitor =
  getChannelMap >>= (liftIO.createHandleAndMonitorIO)

cloneMonitor :: MonadIO m => EventMonitor -> m EventMonitor
cloneMonitor evMonitor = liftIO $ atomically $ EventMonitor <$> cloneTChan (evChannel evMonitor)

cloneMonitorSTM :: EventMonitor -> STM EventMonitor
cloneMonitorSTM evMonitor =  EventMonitor <$> cloneTChan (evChannel evMonitor)
