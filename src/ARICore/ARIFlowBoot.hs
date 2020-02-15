module ARICore.ARIFlowBoot (
    ARIConfig (..)
  , ARIEnv
  , initARI
  , mkHTTPEnvFromConfig
  , createFlowDataEnv
  , initARIAndCreateFlowData
  , submitCmd
  , runARIFlowWithInitState
  , runFlowWithContext
  , runFlowWithContext'
  , startFlowContext
  ) where

import Protolude
import Prelude (String)
import qualified Network.WebSockets  as WS
import Network.Socket      (withSocketsDo)
import Control.Concurrent.STM.TChan

import Utils

import ARICore.HTTPCommand
import ARICore.EventDispatcher
import ARICore.ARIFlow

data ARIConfig=ARIConfig {  appName::String
                         ,  serverAddress::String
                         ,  userName::String
                         ,  pwd::String
                         ,  port::Int
                         --,  newChannelHandler::NewChannelHandler
                         }

data ARIEnv =ARIEnv
              { _ariHTTPEnv::HTTPEnv
              , _ariEventDispData::EventDispData
              , _wsThreadID::ThreadId
              }

loggerPath::String
loggerPath=getLoggerPath "ARIFlowBoot"

mkHTTPEnvFromConfig:: ARIConfig-> HTTPEnv
mkHTTPEnvFromConfig config = 
  let httpURL=mconcat ["http://" , serverAddress config ,":",  (show.port) config, "/ari/"]
  in mkHTTPEnv (appName config) httpURL (userName config) (pwd config)

initARI::ARIConfig-> NewChannelHandler -> IO ARIEnv
initARI config newChannelHandler = do
  channelsMap <- createChannelMap
  let httpEnvir=mkHTTPEnvFromConfig config
      evDispData= channelsMap
      wsPath=mconcat ["/ari/events?api_key=", userName config , ":", pwd config ,"&app=", appName config]

      wsDispatcher::WS.Connection->IO ()
      wsDispatcher conn =(runReaderT (createEventDispatcher conn newChannelHandler)) evDispData
  threadID <- forkIO $ withSocketsDo $ WS.runClient (serverAddress config) (port config) wsPath wsDispatcher
  infoM loggerPath "ARI init completed"
  return $ ARIEnv httpEnvir evDispData threadID


createFlowDataEnv :: ARIEnv -> IO (FlowData s)
createFlowDataEnv  (ARIEnv httpEnvir evDispData _) = createFlowData httpEnvir evDispData

initARIAndCreateFlowData :: ARIConfig -> NewChannelHandler -> IO (FlowData s)
initARIAndCreateFlowData cfg newChannelHandler = initARI cfg newChannelHandler >>= createFlowDataEnv

-- can be used when fd even should not be exposed
runARIFlowWithInitState :: ARIEnv -> s -> IO (Either FlowError (), s)
runARIFlowWithInitState ariEnv s = do
  fd <- createFlowDataEnv ariEnv
  runFlowWithInitState fd s

runFlowWithContext::FlowContext s a -> ARIEnv -> s -> IO (Either FlowError (), s)
runFlowWithContext flowContext ariEnv s = do
  fd <- createFlowDataEnv ariEnv
  runFlowLoop flowContext fd s

runFlowWithContext' :: (EmptyData s) => FlowContext s a -> ARIEnv -> IO (Either FlowError (), s)
runFlowWithContext' flowContext ariEnv = runFlowWithContext flowContext ariEnv emptyData

startFlowContext :: (EmptyData s) => ARIConfig -> NewChannelHandler -> FlowContext s () -> IO (FlowContext s () -> IO ())
startFlowContext config newChannelHandler flowContext = do
  fd <- initARIAndCreateFlowData config newChannelHandler

  _async <- async $ runFlowLoop' flowContext fd

  infoM loggerPath ("Started"::String)

  return $ submitCmd fd

submitCmd ::  FlowData s -> FlowContext s a->IO ()
submitCmd fd hnd =
  atomically $ writeTChan (notifChannel fd) (hnd >> return True)

