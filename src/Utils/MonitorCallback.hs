{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Utils.MonitorCallback
  ( monitorCallbackWithTrans
  , monitorCallbackFilter
  , monitorCallback
  ) where

import Protolude
import Prelude (String)

import Utils.EventDistr
import Utils.CallbackCont
import Utils.LoggerSetup

loggerPath::String
loggerPath=getLoggerPath "MonitorCallback"

monitorCallbackFilter :: ((MonadState s) m, MonadIO m) => EventDistributorLens ev m s -> (ev->Bool) -> CallbackContT m ev
monitorCallbackFilter evDistr filterFunc =
  createCallbackCont (\cbFunc-> do
    monID <- addMonitor evDistr (EventMonitor filterFunc  (\_ ev -> (cbFunc ev >> return False)))
    liftIO $ debugM loggerPath ("monitorCallbackFilter added:" <> show monID)
    return $ removeMonitor evDistr monID)

monitorCallback :: ((MonadState s) m, MonadIO m) => EventDistributorLens ev m s -> CallbackContT m ev
monitorCallback evDistr = monitorCallbackFilter evDistr (const True)

monitorCallbackWithTrans :: ((MonadState s) m, MonadIO m) => EventDistributorLens ev m s -> (ev->Maybe ev') -> CallbackContT m ev'
monitorCallbackWithTrans evDistr transEv =
  createCallbackCont (\cbFunc-> do
    monID <- addMonitorWithTrans evDistr transEv (\monID ev' -> cbFunc ev' >> liftIO (debugM loggerPath ("monitorCallbackWithTrans (handled) monID:" <> show monID)) >> return False)
    liftIO (debugM loggerPath ("monitorCallbackWithTrans monitorAdded monID:" <> show monID))
    return (removeMonitor evDistr monID >> liftIO (debugM loggerPath ("monitorCallbackWithTrans monitorRemoved monID:" <> show monID))))
