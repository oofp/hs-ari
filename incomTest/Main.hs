{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Prelude (String)

import Protolude
import Utils
import ARICore

applName::String
applName="hello-world"

finishFlow :: a -> FlowContext s ()
finishFlow _ = do
  liftIO $ putStrLn ("Completed"::Text)
  exitFlow

logResultAndFinishFlow :: (Show a)=>a -> FlowContext s ()
logResultAndFinishFlow res = do
  liftIO $ infoM rootLog (("Completed with"::String) <> show res)
  exitFlow

--'  
--incomingChannelHandler = 

config :: String -> ARIConfig
config srvAddress = ARIConfig applName srvAddress "boris" "boris" 8088 -- incomingChannelHandler

ariEventHandler :: ARIEvent -> ChannelHandle -> FlowContext () Bool
ariEventHandler ariEvent channelHandle = do
  liftIO $ putStrLn (("ariEventHandler"::Text) <> show ariEvent) >> return True
  case ariEvent of 
    ChannelEvent (ChannelEventData StasisStart _) -> 
      answerChannelCmd channelHandle >> return True
    ChannelEvent (ChannelEventData StasisEnd _) -> return False
    ChannelEvent (ChannelEventData ChannelDestroyed _) -> return False
    ChannelEvent (ChannelEventData ChannelHangupRequest _) -> return False
    _ -> return True

main :: IO ()
main = do
  setupLog2
  infoM rootLog "Starting sipTest"
  let cfg = config "192.168.2.200"
      httpEnv = mkHTTPEnvFromConfig cfg
      incomingChannelHandler = createIncomingChannelHandler httpEnv ariEventHandler (\_->return True) () 
  _ariEnv <- initARI cfg incomingChannelHandler
  putStrLn ("Pres <Enter> to exit"::Text)
  void $ getLine
