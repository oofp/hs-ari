{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--  cmd <- startFlowContext appContainer
--  let hndReq rq = cmd $ handleRequest rq
--  let sipAccount = AccountConfig "username" "pwd" "domain" Nothing
--  hndReq $ SetSIPAccount sipAccount
--  let dialCfg = DialConfig "106" (Just "107") Nothing Nothing
--  hndReq $ SetDialConfig dialCfg
--  let mc= FlowAppComps.BasicCallApp.MakeCall (FlowAppComps.BasicCallApp.MakeCallParams "18001234567")
--  hndReq $ BCRequest mc

module FlowAppComps.FlowAppContainerData
  ( Request (..)
  , RequestData (..)
  , BC.BasicCallReq (..)
  , Event (..)
  , printBasicCallData
  , printFlowAppData
  ) where

import Protolude
import Elm.Derive
import Elm.Module

import ARICore
import Utils

import qualified FlowAppComps.BasicCallApp as BC
import qualified FlowAppComps.TeamCall.TeamCallData as TD
-- import qualified FlowAppComps.BasicCallApp2 as BC2

import FlowAppComps.FlowAppHelpers

data RequestData = SetSIPAccount (Maybe AccountConfig)
                 | SetDialConfig (Maybe DialConfig)
                 | BCRequest BC.BasicCallReq
                 | TCRequest TD.TeamRequest
                 | GetSnapshot
--                 | BCRequest2 BC2.BasicCallReq2
                 deriving (Show, Generic)
--instance ToJSON RequestData
--instance FromJSON RequestData
deriveBoth defaultOptions ''RequestData

data Request = Request {reqID::Text, reqData::RequestData} deriving (Show, Generic)
-- instance ToJSON Request
-- instance FromJSON Request
deriveBoth defaultOptions ''Request

data Event = RequestConf Text
           | BCEvent BC.BasicCallRS
           | TCEvent TD.TeamEvent
           deriving (Show, Generic)
-- instance ToJSON Event
-- instance FromJSON Event
deriveBoth defaultOptions ''Event

printBasicCallData :: IO ()
printBasicCallData =  putStrLn $ makeElmModule "BasicCallData"
    [ DefineElm (Proxy :: Proxy BC.DestParty)
    , DefineElm (Proxy :: Proxy BC.BasicCallReq)
    , DefineElm (Proxy :: Proxy BC.BasicCallRS)
    ]

printFlowAppData :: IO ()
printFlowAppData =  putStrLn $ makeElmModule "FlowAppData"
    [ DefineElm (Proxy :: Proxy ReservedData)
    , DefineElm (Proxy :: Proxy DialConfig)
    , DefineElm (Proxy :: Proxy AccountConfig)
    , DefineElm (Proxy :: Proxy RequestData)
    , DefineElm (Proxy :: Proxy Request)
    , DefineElm (Proxy :: Proxy Event)
    ]
