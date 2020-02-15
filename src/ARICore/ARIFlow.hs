module ARICore.ARIFlow
(
  FlowData (..),
  FlowContext,
  FlowContextM,
  FlowSubContextM,
  FlowSubContextMT,
  FlowContextState,
  FlowHandler,
  FlowError (..),
  ExtCom,
  HandleAsync,
  ARIEventHandler,
  createHandleAndMonitorFlow,
  createMonitorFlowForHanlde,
  createIncomingChannelHandler,
  createHandleFlow,
  releaseHandleFlow,
  releaseHandleOnlyFlow,
  runSubFlow,
  runFlowLoop,
  runFlowLoop',
  runFlowLoopEx,
  runFlowWithInitState,
  runStatelessFlow,
  createFlowData,
  cloneFlowData,
  cloneFlowDataInContext,
  getFlowState,
  getNotifChannel,
  getFlowSink,
  getFlowData,
  spawnFlowWithExtCom,
  (>>~),
  upFlow,
  unfail,
  handleBoth,
  handleBoth_,
  submitTaskIO
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Protolude
import Control.Concurrent.STM.TChan
import Prelude (String)

import Utils.LoggerSetup
import Utils.MiscUtils
import Utils.TChanUtils

import ARICore.EventDispatcher
import ARICore.ARIData
import ARICore.HTTPCommand

data FlowError = HTTPErr HTTPError
               | WrongFlowState Text
               | GenericError Text
               | ChannelEnded
               deriving (Show)

instance Eq FlowError where
  (==) (HTTPErr _) (HTTPErr _) = True
  (==) (WrongFlowState txt1) (WrongFlowState txt2) = txt1==txt2
  (==) (GenericError txt1) (GenericError txt2) = txt1==txt2
  (==) ChannelEnded ChannelEnded = True
  (==) _ _ = False

data FlowData s = FlowData
              { httpEnv::HTTPEnv
              , eventDispData::EventDispData
              --, callsStore::CallsStore
              , notifChannel::TChan (FlowHandler s)
              }

instance HTTPEnvProvider (FlowData s) where
  getHTTPEnv = httpEnv

instance FromHTTPError FlowError where
  fromHTTPError=HTTPErr

loggerPath::String
loggerPath=getLoggerPath "AIRFlow"

type FlowContextM s = ExceptT FlowError (StateT s (ReaderT (FlowData s) IO))
type FlowContext s a= FlowContextM s a
type FlowContextState s a=StateT s (ReaderT (FlowData s) IO) (Either FlowError a)

type FlowSubContextMT m1 s subs r =  ExceptT FlowError (StateT subs (ReaderT r (m1 (FlowContextM s))))
type FlowSubContextM s subs r =  FlowSubContextMT IdentityT s subs r
type FlowSubContext s subs r a = FlowSubContextM s subs r a

upFlow::FlowContextState s a -> (Either FlowError a->Either FlowError b)->FlowContext s b
upFlow st f = ExceptT $ fmap f st

(>>~)::FlowContextState s a -> (Either FlowError a->Either FlowError b)->FlowContext s b
(>>~) = upFlow

unfail::FlowContext s a->FlowContext s (Either FlowError a)
unfail fc= catchError (Right <$> fc) (return.Left)

handleBoth::FlowContext s a->(FlowError->FlowContext s b)->(a->FlowContext s b)->FlowContext s b
handleBoth fc handleError handleResult= do
  res <- unfail fc
  case res of
    (Left fe) -> handleError fe
    (Right a) -> handleResult a

handleBoth_::FlowContext s a->FlowContext s b->FlowContext s b->FlowContext s b
handleBoth_ fc handleError handleResult= do
  res <- unfail fc
  case res of
    (Left _) -> handleError
    (Right _) -> handleResult


type FlowHandler s=FlowContext s Bool
type FlowCompleted s=FlowContext s ()
type ARIEventHandler s h = (ARIEvent -> h -> FlowContext s Bool)
type FlowSink s = FlowHandler s -> IO ()

getFlowState::FlowContext s s
getFlowState = lift S.get

getFlowData::FlowContext s (FlowData s)
getFlowData = (lift.lift)  R.ask

getNotifChannel::FlowContext s (TChan (FlowContext s Bool))
getNotifChannel = do
  fd <- (lift.lift)  R.ask
  return $ notifChannel fd

getFlowSink::FlowContext s (FlowSink s)
getFlowSink =  do
  tchan <- getNotifChannel
  return $ \handler -> atomically $ writeTChan tchan handler

-- lift EventDispatcher commands to FlowContext
fromEventDisp:: EventDisp a -> FlowContext s a
fromEventDisp = transformReaderToStateExcept eventDispData

createHandleFlow :: ARIHandle a => FlowContext s a
createHandleFlow = fromEventDisp createHandle

type HandleAsync a= (a, Async ())

createHandleAndMonitorFlow :: ARIHandle a => ARIEventHandler s a -> FlowContext s (HandleAsync a)
createHandleAndMonitorFlow eventHandler = do
    (hnd, evMon) <- fromEventDisp createHandleAndMonitor
    asyncLoop <- spawnReadEventsLoop evMon eventHandler hnd
    return (hnd , asyncLoop)

createMonitorFlowForHanlde :: ARIHandle a => ARIEventHandler s a -> a -> FlowContext s (Maybe (Async ()))
createMonitorFlowForHanlde eventHandler hnd= do
   evMonMaybe<- fromEventDisp $ newMonitorForHandle hnd
   case evMonMaybe of
     Nothing -> return Nothing
     (Just evMon) -> Just <$> spawnReadEventsLoop evMon eventHandler hnd

spawnReadEventsLoop :: ARIHandle a => EventMonitor ->  ARIEventHandler s a  -> a -> FlowContext s (Async ())
spawnReadEventsLoop (EventMonitor ariChan) eventHandler hnd= do
   notificChannel <- getNotifChannel
   liftIO $ async $ readEvents notificChannel
 where
   readEvents notificChannel=
     do (ev, plc) <- readTChanIO  ariChan
        infoM loggerPath ("spawnReadEventsLoop" <> show ev)
        atomically $ writeTChan notificChannel (eventHandler ev hnd)
        unless (plc==FinalEvent) (readEvents notificChannel)

createFlowData:: HTTPEnv -> EventDispData ->IO (FlowData s)
createFlowData httpEnvir evDispData = do
    notificChannel <- newTChanIO
    return $ FlowData httpEnvir evDispData notificChannel
        
--type NewChannelHandler=ChannelEventData -> ChannelHandle -> EventMonitor ->IO ()
--fd <- createFlowData ariEnv
createIncomingChannelHandler :: HTTPEnv -> ARIEventHandler s ChannelHandle -> (ChannelHandle -> FlowContext s Bool) -> s -> NewChannelHandler  
createIncomingChannelHandler httpEnvir ariEventHandler flowHandler s 
    evDispData -- EventDispData  
    _chEvData -- eventType and Channel details 
    channelHandle -- channelID
    evMonitor -- wrapped tchan
    = do
  infoM loggerPath ("createIncomingChannelHandler entered")
  fd <- createFlowData httpEnvir evDispData    
  let sumFlow = spawnReadEventsLoop evMonitor ariEventHandler channelHandle >> flowHandler channelHandle >> flowLoop
  void $ runFlow sumFlow fd s    
  infoM loggerPath ("createIncomingChannelHandler left")

releaseHandleFlow::ARIHandle a => HandleAsync a -> FlowContext s ()
releaseHandleFlow (hnd, asyncLoop) = fromEventDisp (releaseHandle hnd) >> liftIO (cancel asyncLoop)

-- must be used only if events are not monitored (only for bridge)
releaseHandleOnlyFlow::BridgeHandle -> FlowContext s ()
releaseHandleOnlyFlow hnd = fromEventDisp (releaseHandle hnd)

--newEventMonitor :: ARIHandle a => a -> FlowContext s (Maybe EventMonitor)
--newEventMonitor hnd = fromEventDisp $ newMonitorForHandle hnd

runFlow::FlowContext s a->FlowData s->s->IO (Either FlowError a, s)
runFlow flowContext fd st =
    let stateT = runExceptT flowContext
        stateFunc = runStateT stateT
        readerT = stateFunc st
        readerFunc = runReaderT readerT
    in  readerFunc fd

runSubFlow::FlowSubContext s subs r a->r->subs->FlowContext s (Either FlowError a, subs)
runSubFlow flowSubContext r subst =
    let stateT = runExceptT flowSubContext
        stateFunc = runStateT stateT
        readerT = stateFunc subst
        readerFunc = runReaderT readerT
    in  runIdentityT $ readerFunc r

showError::FlowError->String
showError = show

flowLoop :: FlowCompleted s
flowLoop = do
    tchan <- getNotifChannel
    readChannelLoop tchan
  where
    safeHandler::FlowHandler s-> FlowHandler s
    safeHandler handler = catchError handler
      (\err-> do
        liftIO $ errorM loggerPath ("Unhandled error:"<> showError err)
        return True
      )
    readChannelLoop tchan = do
      handler <- liftIO $ atomically $ readTChan tchan
      flContinue <- safeHandler handler
      when flContinue (readChannelLoop tchan)

runFlowLoop::FlowContext s a->FlowData s->s->IO (Either FlowError (), s)
runFlowLoop flowContext = runFlow (flowContext>>flowLoop)

runFlowLoop' :: (EmptyData s) => FlowContext s a-> FlowData s-> IO (Either FlowError (), s)
runFlowLoop' flowContext fd = runFlow (flowContext>>flowLoop) fd emptyData

emptyFlowContext :: FlowContext s ()
emptyFlowContext = return ()

runFlowWithInitState :: FlowData s -> s -> IO (Either FlowError (), s)
runFlowWithInitState  = runFlowLoop emptyFlowContext -- fd s

runStatelessFlow :: FlowData () -> IO (Either FlowError (), ())
runStatelessFlow fd = runFlowWithInitState fd ()

runFlowLoopEx::IO () -> FlowContext s a -> FlowData s ->s->IO (Either FlowError (), s)
runFlowLoopEx onDone flowContext fd st = do
  res <- runFlowLoop flowContext fd st
  onDone
  return res

cloneFlowData::FlowData s1-> IO (FlowData s2)
cloneFlowData (FlowData httpEv evDispData _) = do
  newChannel <- newTChanIO
  return $ FlowData httpEv evDispData newChannel

cloneFlowDataInContext :: FlowContext s1 (FlowData s2)
cloneFlowDataInContext =
   getFlowData >>= liftIO . cloneFlowData

submitTaskIO :: FlowData s -> FlowHandler s -> IO ()
submitTaskIO fd task = atomically $ writeTChan (notifChannel fd) task

type ExtCom a =(TChan a, TChan a)
spawnFlowWithExtCom::(ExtCom d->FlowData s->IO (Either FlowError (),a))->FlowData s->IO (ExtCom d, Async (Either FlowError (),a))
spawnFlowWithExtCom runflowContextWithExtFunc fd= do
  infoM loggerPath "spawnFlowWithExtCom entered"
  chan1 <- newTChanIO
  chan2 <- newTChanIO
  let extCom=(chan1,chan2)
  infoM loggerPath "going to runflowContextWithExtFunc..."
  asyncRes <- async $ runflowContextWithExtFunc extCom fd
  infoM loggerPath "runflowContextWithExtFunc async-ed"
  return (extCom,asyncRes)


