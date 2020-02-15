module FlowTools.FlowCallback
( FlowCallback
, FlowCallbackM
, FlowSubCallback
, FlowSubCallbackM
, InvokeCallCompFunc
, runSubCall
, spawnFlowComp
, runCallComp
, evalCallComp
, execCallComp
, onCancel
, onCancel_
, onCancelAsync
, liftFCxt
, liftFCb
) where

import Utils
import ARICore

import Protolude
import Control.Concurrent.STM.TChan

type FlowCallbackM s = CallbackContT (FlowContextM s)
type FlowCallback s a = (FlowCallbackM s) a

type FlowSubCallbackM s subs r = FlowSubContextMT CallbackContT s subs r
type FlowSubCallback s subs r a = (FlowSubCallbackM s subs r) a

runSubCall :: FlowSubCallback s subs r a -> r -> subs -> FlowCallback s (Either FlowError a, subs)
runSubCall flowSubCallback r subst=
      let stateT = runExceptT flowSubCallback
          stateFunc = runStateT stateT
          readerT = stateFunc subst
          readerFunc = runReaderT readerT
      in  readerFunc r

liftFCxt :: FlowContext  s a -> FlowSubCallback s subs r a
liftFCxt = lift.lift.lift.lift

liftFCb :: FlowCallback  s a -> FlowSubCallback s subs r a
liftFCb = lift.lift.lift

spawnFlowComp' :: FlowContext s1 a -> s1 -> FlowCallback s (Either FlowError (), s1)
spawnFlowComp' newFlow initState = createCallbackCont  (\cbFunc -> do
    fd <- cloneFlowDataInContext
    notifChan <- getNotifChannel
    asyncRes <- liftIO $ async $ runFlowLoop newFlow fd initState >>= (\res -> atomically $ writeTChan notifChan (cbFunc res >> continue))
    return $ liftIO $ cancel asyncRes)

spawnFlowComp :: (EmptyData s1) => FlowContext s1 a -> FlowCallback s (Either FlowError (), s1)
spawnFlowComp newFlow = spawnFlowComp' newFlow emptyData

type InvokeCallCompFunc s s1 a a_s1 = FlowCallback s1 a -> FlowContext s1 () -> s1 -> FlowCallback s a_s1

runCallComp :: FlowCallback s1 a -> FlowContext s1 () -> s1 -> FlowCallback s (a, s1)
runCallComp call1 initFlow1 initState1 = createCallbackCont  (\cbFunc -> do
  fd1 <- cloneFlowDataInContext
  fd <- getFlowData
  cc1 <- newForeignCallContext
  let callFlow=runCallWithContext cc1 call1 (\a-> do
        s1 <- lift get
        liftIO $ submitTaskIO fd (cbFunc (a,s1) >> continue))
      sumFlow = initFlow1 >> callFlow
  _asyncRes <- liftIO $ async $ runFlowLoop sumFlow fd1 initState1
  return $ liftIO $ submitTaskIO fd1 (cancelCall cc1 >> return False))

evalCallComp :: FlowCallback s1 a -> FlowContext s1 () -> s1 -> FlowCallback s a
evalCallComp call1 initFlow1 initState1 = fst <$> runCallComp call1 initFlow1 initState1

execCallComp :: FlowCallback s1 a -> FlowContext s1 () -> s1 -> FlowCallback s s1
execCallComp call1 initFlow1 initState1 = snd <$> runCallComp call1 initFlow1 initState1

onCancel :: (s ->FlowContext s ()) -> FlowCallback s a -> FlowCallback s a
onCancel onCancelFunc flowCallback = createCallbackCont (\cbFunc-> do
  cc <- runCall flowCallback cbFunc
  return $ do
    s <- lift get
    cancelCall cc
    onCancelFunc s)

onCancel_ :: FlowContext s () -> FlowCallback s a -> FlowCallback s a
onCancel_ onCancelFunc_  = onCancel (\_->onCancelFunc_)

onCancelAsync :: (s -> IO ()) -> FlowCallback s a -> FlowCallback s a
onCancelAsync onCancelIOFunc = onCancel (liftIO . void . async . onCancelIOFunc) -- flowCallback
