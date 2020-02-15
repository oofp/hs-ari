{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--  cmd <- startFlowContext appContainer
--  let hndReq rq = cmd $ handleRequest rq
--  let sipAccount = AccountConfig "user" "pwd" "domain" Nothing
--  hndReq $ SetSIPAccount sipAccount
--  let dialCfg = DialConfig "106" (Just "106") Nothing Nothing
--  hndReq $ SetDialConfig dialCfg
--  let mc= FlowAppComps.BasicCallApp.MakeCall (FlowAppComps.BasicCallApp.MakeCallParams "18001234567")
--  hndReq $ BCRequest mc

module FlowAppComps.FlowAppContainer
  ( FlowAppContainer
  , Request (..)
  , RequestData (..)
  , BC.BasicCallReq (..)
  , runAppContainer
  , appContainer
  , handleRequest
  ) where

import Protolude
import Prelude (String)
import Data.Aeson hiding ((.=))
import Control.Lens

import Utils
import ARICore
import FlowTools

import qualified FlowAppComps.BasicCallApp as BC
import qualified FlowAppComps.TeamCall.TeamCallApp  as TC
-- import qualified FlowAppComps.BasicCallApp2 as BC2
import qualified Data.ByteString.Lazy as LBS

import FlowAppComps.FlowAppHelpers
import FlowAppComps.FlowAppContainerData

loggerPath::String
loggerPath=getLoggerPath "FlowAppContainer"

data AppContainerState = AppContainerState
        { _sipAccount :: PJSIPAccountData
        , _dialCfg :: Maybe DialConfig
        , _ariEvDistr :: ARIEventDistributor AppContainerState
        , _bcAppState :: BC.AppState AppContainerState
        , _tcAppState :: TC.AppState AppContainerState
--        , _bcAppState2 :: BC2.AppState2 AppContainerState
        , _extCom :: ExtComData LBS.ByteString AppContainerState
        , _extComStateDistr :: EventDistributor Bool (FlowContextM AppContainerState) AppContainerState
        }

makeLenses ''AppContainerState

instance HasARIEventDistributor AppContainerState where
  getEventDistributor = ariEvDistr

instance  HasPJSIPAccount AppContainerState where
  pjSIPAccount = sipAccount

instance HasDialConfig AppContainerState where
  dialConfig = dialCfg

instance BC.HasAppState AppContainerState where
  subSvc = bcAppState
instance TC.HasAppState AppContainerState where
  subSvc = tcAppState
instance EmptyData AppContainerState where
  emptyData = newAppContainerState

instance HasExtCom AppContainerState LBS.ByteString where
  extComTool = extCom

newAppContainerState :: AppContainerState
newAppContainerState = AppContainerState emptySIPAccount Nothing newEventDistributor emptyData emptyData (newExtCom "extComAppContainer" extMsgHandler extComStateChanged) newEventDistributor
{-
instance HasSubSvc BC.BasicCallRS BC.BasicCallReq (BC.AppState AppContainerState) AppContainerState where
  subSvc = bcAppState

instance HasSubSvc BC2.BasicCallRS2 BC2.BasicCallReq2 (BC2.AppState2 AppContainerState) AppContainerState where
  subSvc = bcAppState2
-}

type FlowAppContainer a = FlowContext AppContainerState a

handleRequest :: RequestData -> FlowAppContainer ()
handleRequest requestData =
  case requestData of
    SetSIPAccount (Just accountCfg) -> setPJSIPAccount accountCfg
    SetSIPAccount Nothing -> resetPJSIPAccount
    SetDialConfig dialingCfg -> dialCfg .= dialingCfg
    BCRequest bcReq -> dispatchSvcRequest bcAppState bcReq
    TCRequest tcReq -> dispatchSvcRequest tcAppState tcReq
    -- BCRequest2 bcReq2 -> dispatchSvcRequest bcAppState2 bcReq2
    GetSnapshot -> getSnapshot

getSnapshot :: FlowAppContainer ()
getSnapshot =
  -- in future send state only of services that are not null.
  -- probably introduce type class isNullOrSmthng to see if state is null
  snapshot <$> use bcAppState >>= \sshot-> sendEvent $ BCEvent sshot

extMsgHandler :: ExtMsgHandler LBS.ByteString AppContainerState
extMsgHandler msgBytes =
  let req :: Maybe Request
      req = decode msgBytes
  in do
    case req of
      Nothing -> liftIO $ warningM loggerPath ("Failed to parse:" <> show msgBytes)
      Just (Request requestID requestData) -> do
        handleRequest requestData
        sendEvent (RequestConf requestID)
    return True

extComStateChanged :: ExtComStateChanged AppContainerState
extComStateChanged flOnOff = do
  liftIO $ infoM loggerPath ("extComStateChanged:" <> show flOnOff)
  notifyEvent extComStateDistr flOnOff

sendEvent :: Event -> FlowContext AppContainerState ()
sendEvent ev = sendMessage extCom (encode ev)

handleSubSvcEvent :: (FlowSubSvc ev rq subs AppContainerState) => Lens' AppContainerState subs -> (ev -> Event) -> FlowAppContainer ()
handleSubSvcEvent subSrv evTransFunc =
  void $ monitorSvcEvent'  subSrv evTransFunc sendEvent

extComMonCall :: FlowCallback AppContainerState ()
extComMonCall =
  let monComUp = monitorCallbackFilter extComStateDistr (==True)
      monComDown = monitorCallbackFilter extComStateDistr (==False)
      monCycle = do
        void monComDown
        waitForUpRes <- raceCall (timeCallback 900000000) monComUp
        case  waitForUpRes of
          Left _ -> lift terminateFlow
          Right _ -> monCycle
  in  monCycle

terminateFlow :: FlowContext AppContainerState ()
terminateFlow = resetPJSIPAccount >> exitFlow

appContainer :: FlowAppContainer ()
appContainer = do
  handleSubSvcEvent bcAppState BCEvent
  handleSubSvcEvent tcAppState TCEvent
  void $ runCall BC.basicCallApp (\()->logCallCompleted "basicCallApp")
  void $ runCall TC.teamCallApp (\()->logCallCompleted "teamCallApp")
  void $ runCall extComMonCall (\()->logCallCompleted "extComMonCall")
  -- void $ runCall BC2.basicCallApp2 (\()->logCallCompleted "basicCallApp2")

logCallCompleted :: String -> FlowContext AppContainerState ()
logCallCompleted  callTxt =
  liftIO $ warningM loggerPath ("Call completed:" <> callTxt)

runAppContainer :: FlowData AppContainerState->IO (Either FlowError (), AppContainerState)
runAppContainer fd = runFlowLoop appContainer fd  newAppContainerState
