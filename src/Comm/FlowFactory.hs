
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Comm.FlowFactory
    ( flowScenarioFactory
    ) where


import Protolude
import Comm.CommGate

import Utils
import ARICore
import FlowTools

import Prelude (String)
import qualified Data.ByteString.Lazy as LBS

loggerPath::String
loggerPath=getLoggerPath "FlowFactory"

type LBStr =LBS.ByteString

--type ScenarioFactory = ChannelsPair -> IO () -> IO ChannelsUpdater
flowScenarioFactory :: (EmptyData s, HasExtCom s LBStr) => ARIEnv -> FlowContext s () -> ScenarioFactory
flowScenarioFactory env flowContext extCom scenarioCompleted = do
  infoM loggerPath "Entered flowScenarioFactory"
  fd <-createFlowDataEnv env
  infoM rootLog "flowScenarioFactory, fd created"

  _asyncRes <- async $ runFlowLoopEx scenarioCompleted flowContext fd emptyData
  --let notificationChannel=notifChannel fd
  --let extComUpdater = (\newExtCom -> atomically $ writeTChan notificationChannel (setExtCom extComTool newExtCom))
  let extComUpdater :: ChannelsUpdater
      extComUpdater extComm = submitCmd fd (setExtCom extComTool extComm)
  extComUpdater extCom
  return extComUpdater
