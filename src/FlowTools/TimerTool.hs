module FlowTools.TimerTool
  ( startTimer
  , timeCallback
  ) where

import Control.Concurrent.Thread.Delay
import Protolude
import Control.Concurrent.STM.TChan

import Utils
import ARICore

import FlowTools.FlowCallback

startTimer :: Integer ->  FlowHandler s -> FlowContext s (Async ())
startTimer timeout timerHandler = do
  tchan <- getNotifChannel
  liftIO $ async $ do
    delay timeout
    atomically $ writeTChan tchan timerHandler


timeCallback :: Integer -> FlowCallback s ()
timeCallback timeout = createCallbackCont  (\cBack -> do
  asyncTimer <- startTimer timeout (cBack () >> continue)
  return $ liftIO $ cancel asyncTimer )
