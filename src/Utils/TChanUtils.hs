module Utils.TChanUtils where

import Control.Monad
import Control.Concurrent.STM.TChan
import Protolude

waitForEventOrg :: (a -> Bool) -> TChan a -> STM a
waitForEventOrg p chan = do
  event <- readTChan chan
  if p event
          then return event
          else retry

waitForEvent' :: (a -> Bool) -> TChan a -> STM a
waitForEvent' p chan = checkEvent
    where
      checkEvent = do
        event <- readTChan chan
        if p event
            then return event
            else checkEvent

waitForEvents :: Int->TChan a -> STM [a]
waitForEvents num chan = replicateM num (readTChan chan)

readTChanIO::TChan a->IO a
readTChanIO ch=atomically $ readTChan ch
