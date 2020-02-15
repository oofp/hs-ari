{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FlowTools.ExtComTool
    (
      newExtCom,
      clearExtCom,
      sendMessage,
      setExtCom,
      ExtComData,
      ExtMsgHandler,
      ExtComStateChanged,
      HasExtCom (..)
    ) where

import Protolude
import Prelude (String)

import Control.Concurrent.STM.TChan
import qualified GHC.Show (Show (..))
import Control.Lens
import Data.Text

import Utils
import ARICore

loggerPath::String
loggerPath=getLoggerPath "ExtCom"

type ExtMsgHandler d s = d->FlowHandler s
type ExtComStateChanged s = Bool -> FlowContext s ()

data ChannLoopData d = ChannLoopData
  { outChan   :: TChan d
  , _inChan    :: TChan d
  , asyncTask :: Async ()
  }

cancelLoop :: ChannLoopData d -> IO ()
cancelLoop loopData = cancel (asyncTask loopData)

type This d s = Lens' s (ExtComData d s)

data ExtComData d s = ExtComData { _name :: Text,  _chanLoopData :: Maybe (ChannLoopData d),  _msgHnd :: ExtMsgHandler d s,  _stateChanged ::ExtComStateChanged s}

makeLenses ''ExtComData

class HasExtCom s d where
  extComTool :: Lens' s (ExtComData d s)

instance Show (ExtComData d s) where
  show extCom=unpack $ "ExtCom [" <> _name extCom <> "] "

debugLog :: This d s -> String -> FlowContext s ()
debugLog this logMsg = do
  extComData <- use this
  liftIO $ debugM loggerPath (show extComData <> ":" <> logMsg)

--return just plain data (not in manadic context)
newExtCom :: Text -> ExtMsgHandler d s -> ExtComStateChanged s -> ExtComData d s
--newExtCom namePar msgHndPar extComReplacedPar = ExtComData  namePar Nothing msgHndPar extComReplacedPar
newExtCom namePar  = ExtComData  namePar Nothing

clearExtCom:: This d s -> FlowContext s ()
clearExtCom this = do
  chanLoopMaybe <- use (this.chanLoopData)
  liftIO $ forM_  chanLoopMaybe cancelLoop
  this.chanLoopData .= Nothing

handleConnectionTerminated :: This d s -> FlowContext s ()
handleConnectionTerminated this = do
  this.chanLoopData .= Nothing
  (this.stateChanged) .$ ($ False)

sendMessage:: (Show d) => This d s -> d -> FlowContext s ()
sendMessage this msgFromFlow = do
  liftIO $ debugM loggerPath ("sendMessage:" <> show msgFromFlow)
  chanLoopMaybe <- use (this.chanLoopData)
  liftIO $ forM_ chanLoopMaybe (\chanLoop -> atomically $ writeTChan (outChan chanLoop) msgFromFlow)

startReadLoop :: (Eq d, Monoid d) => This d s -> TChan d -> ExtMsgHandler d s -> FlowContext s (Async ())
startReadLoop this inputChannel extMsgHandler = do
  flowChannel <- getNotifChannel
  liftIO $ async $ doWhile $ atomically $ do
    incomingMsg <- readTChan inputChannel
    if incomingMsg == mempty
      then do
        writeTChan flowChannel (handleConnectionTerminated this >> return True)
        return False
      else do
        writeTChan flowChannel (extMsgHandler incomingMsg)
        return True

setExtCom :: (Eq d, Monoid d) => This d s -> (TChan d, TChan d) -> FlowContext s ()
setExtCom this (fromFlowChannel, toFlowChannel)  = do
  debugLog this "setExtCom"
  clearExtCom this
  msgHandler <- use (this.msgHnd)
  asynTask <- startReadLoop this toFlowChannel msgHandler -- no race condition is possible as notication is going through notifChannel so stateChange will be always True before switching to False
  this.chanLoopData .= (Just $ ChannLoopData fromFlowChannel toFlowChannel asynTask)
  (this.stateChanged) .$ ($ True)
