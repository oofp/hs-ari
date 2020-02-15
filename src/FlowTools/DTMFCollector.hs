{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FlowTools.DTMFCollector
    ( newDTMFCollector
    , startDTMFCollector
    , stopDTMFCollector
    , addDTMFMonitor
    , removeDTMFMonitor
    , getDTMFBuffer
    , clearDTMFBuffer
    , getAndClearDTMFBuffer
    , addAnyDTMFMonitor
    , DTMFCollector
    , DTMFCollectorLens
    , DTMFBuffer
    ) where


import Protolude
import Control.Lens
import Prelude (String)

import ARICore
import Utils
import ARIUtils

import FlowTools.FlowEvDistr

data DTMFDetectedEvent = DTMFDetectedEvent {_dtmfBuffer::DTMFBuffer, _dtmfDigit :: DTMFDigit}

data DTMFCollector s = DTMFCollector {_monID :: Maybe EventID, _dtmfEvDistr :: EventDistributor DTMFDetectedEvent (FlowContextM s) s, _curDTMFBuffer :: DTMFBuffer}
type DTMFCollectorLens s = Lens' s (DTMFCollector s)

makeLenses ''DTMFCollector
makeLenses ''DTMFDetectedEvent

loggerPath::String
loggerPath=getLoggerPath "DTMFCollector"

addDTMFToBuffer :: DTMFDigit -> DTMFBuffer -> DTMFBuffer
addDTMFToBuffer d buf = buf ++ [d]

newDTMFCollector :: DTMFCollector s
newDTMFCollector = DTMFCollector Nothing newEventDistributor emptyDTMDBuffer

startDTMFCollector ::  HasARIEventDistributor s => DTMFCollectorLens s -> ConnectedChannel -> FlowContext s ()
startDTMFCollector this (ChannelInState chanHandle) = do
   curMonID <- use (this.monID)
   case curMonID of
     (Just _) -> liftIO $ warningM loggerPath " already started"
     Nothing -> do
       newMonID <- addARIEventMonitorWithTrans (transEvWithHandle chanHandle digitReceivedFromEvent) (handleDTMFReceived this)
       this.monID .= Just newMonID

handleDTMFReceived :: HasARIEventDistributor s => DTMFCollectorLens s -> evID -> DTMFDigit -> FlowContext s Bool
handleDTMFReceived this _ dtmfReceived = do
  -- (this.curDTMFBuffer) %= (addDTMFToBuffer dtmfReceived)
  curBuf <- this.curDTMFBuffer <%= addDTMFToBuffer dtmfReceived
  notifyEvent (this.dtmfEvDistr) (DTMFDetectedEvent curBuf dtmfReceived)
  return True

stopDTMFCollector :: HasARIEventDistributor s => DTMFCollectorLens s -> FlowContext s ()
stopDTMFCollector this = do
  curMonIDMaybe <- use (this.monID)
  case curMonIDMaybe of
    Nothing -> liftIO $ warningM loggerPath " has not started"
    (Just curMonID) -> ((this.monID) .= Nothing)  >> removeARIEventMonitor curMonID

addDTMFMonitor :: (DTMFDetectedEvent -> Maybe ev') -> EventHandlerFunc s ev' -> DTMFCollectorLens s -> FlowContext s EventID
addDTMFMonitor transFunc evHandlerFunc this  = addMonitorWithTrans (this.dtmfEvDistr) transFunc evHandlerFunc

removeDTMFMonitor :: EventID -> DTMFCollectorLens s -> FlowContext s ()
removeDTMFMonitor evID this = removeMonitor (this.dtmfEvDistr) evID

getDTMFBuffer :: DTMFCollectorLens s -> FlowContext s DTMFBuffer
getDTMFBuffer this = use (this.curDTMFBuffer)

clearDTMFBuffer :: DTMFCollectorLens s -> FlowContext s ()
clearDTMFBuffer this = (this.curDTMFBuffer) .= emptyDTMDBuffer

getAndClearDTMFBuffer :: DTMFCollectorLens s -> FlowContext s DTMFBuffer
getAndClearDTMFBuffer this = (this.curDTMFBuffer) <<.= emptyDTMDBuffer

addAnyDTMFMonitor :: EventHandlerFunc s DTMFDigit -> DTMFCollectorLens s -> FlowContext s EventID
addAnyDTMFMonitor = addDTMFMonitor (\ev->Just $ ev ^. dtmfDigit)
