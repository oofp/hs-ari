{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Utils.CallbackCont
  ( createCallbackCont
  , runCall
  , runCallWithContext
  , raceCall
  , raceCall'
  , callWithOp
  , nestedCall
  , within
  , bothCalls
  , manyCalls
  , infiniteCall
  , altCalls
  , oneOfCall
  , oneOfCall'
  , cancelCall
  , oneOf3Call'
  , compactOneOf3'
  , newRes
  , resourceCall
  , withResource
  , withResourceOut
  , withResourceInOut
  , withResourceIn
  , withResource_
  , newCallContext
  , newForeignCallContext
  , (>:>) -- withResource_ : with release on cancelation and block completion
  , (>|>) -- like withResource_ bit no release on cancelation, only release on block completion
  , (|:>) -- nestedCall
  , (<:|)
  , (>|<)
  , (>||<)
  , CallbackContT (..)
  , OneOf (..)
  , OneOf' (..)
  , OneOf3' (..)
  , ResourceC (..)
  , AllocatedResourceC
  , CallContext
  ) where

import Control.Monad
import Protolude
import Control.Monad.Trans
import Control.Concurrent.STM.TVar
import Utils.MiscUtils

data CallContextData m = CallContextData {cancelFunc::Maybe (m ()) , cancelled ::Bool}
type CallContext m = TVar (CallContextData m)

-- function that takes callback func as parameter , call context and return cancellation function
type CallbackCont m a = (a -> m ()) -> CallContext m -> m ()
newtype CallbackContT m a = CallbackContT {cb :: (a -> m ()) -> CallContext m -> m ()}


createCallbackCont :: MonadIO m => ((a->m ())-> m (m ())) -> CallbackContT m a
createCallbackCont func = CallbackContT $ \cBack cc -> do {cancelFnc <- func (cbUnlessCancelled cc cBack); setCancelFunc cc cancelFnc}

setCancelFunc :: MonadIO m => CallContext m -> m () -> m ()
setCancelFunc cc cancelFnc = liftIO $ atomically $ modifyTVar cc (\(CallContextData _ cced) -> CallContextData (Just cancelFnc) cced)

_resetCancelFunc :: MonadIO m => CallContext m -> m ()
_resetCancelFunc cc = liftIO $ atomically $ modifyTVar cc (\(CallContextData _ cced) -> CallContextData Nothing cced)

isCallCancelled :: MonadIO m => CallContext m -> m Bool
isCallCancelled cc = do
  ccd <- liftIO $ atomically $ readTVar cc
  return $ cancelled ccd

cbUnlessCancelled :: MonadIO m => CallContext m -> (a -> m ()) -> a -> m ()
cbUnlessCancelled cc cbFunc a = do
  cced <- isCallCancelled cc
  unless cced (cbFunc a)

callUnlessCancelled :: MonadIO m => m () -> CallContext m -> m ()
callUnlessCancelled call cc = do
  cced <- isCallCancelled cc
  unless cced call

cancelCall :: MonadIO m => CallContext m -> m ()
cancelCall cc = do
  cFuncMaybe <- liftIO $ atomically $ do
    ccd <- readTVar cc
    writeTVar cc (CallContextData Nothing True)
    return $ cancelFunc ccd
  case cFuncMaybe of
    Nothing -> return ()
    (Just cancelFnc) -> cancelFnc

newCallContext :: MonadIO m => m (CallContext m)
newCallContext = liftIO $ newTVarIO $ CallContextData Nothing False

newForeignCallContext :: (MonadIO m, MonadIO m1)  => m (CallContext m1)
newForeignCallContext = liftIO $ newTVarIO $ CallContextData Nothing False

runCallWithContext ::  MonadIO m => CallContext m -> CallbackContT m a -> (a-> m ()) -> m ()
runCallWithContext  cc (CallbackContT callFunc) cbFunc = callFunc cbFunc cc

runCall ::  MonadIO m => CallbackContT m a -> (a-> m ()) -> m (CallContext m)
runCall (CallbackContT callFunc) cbFunc = do
  cc <- newCallContext
  callFunc cbFunc cc
  return cc

instance MonadIO m => Functor (CallbackContT m) where
  fmap f_ab (CallbackContT  cb_ma) = CallbackContT (\b2m -> cb_ma (b2m.f_ab))

createApplicative :: (MonadIO m) => CallbackContT m (a -> b) -> CallbackContT m a -> CallbackContT m b
createApplicative m_f_ab cb_ma = do
  a2b <- m_f_ab
  a <- cb_ma
  return $ a2b a

instance MonadIO m => Applicative (CallbackContT m) where
  pure a = CallbackContT (\func cc -> callUnlessCancelled (func a) cc) -- should check canncel flag before?
  (<*>)  = createApplicative

a2m :: (MonadIO m) => (a -> CallbackContT m b) -> (b -> m ()) -> CallContext m -> a -> m ()
a2m f2' b2m cc a =
  let (CallbackContT f2) = f2' a
  in callUnlessCancelled (f2 b2m cc) cc -- invoke callback only if not cancelled

-- f1 -first function (one that invokes callback with a)
createCallbackB :: (MonadIO m) =>  CallbackCont m a -> (a -> CallbackContT m b) -> (b -> m ()) -> CallContext m -> m ()
--createCallbackB f1 f2 b2m cc = f1 (a2m f2 b2m cc) cc
createCallbackB f1 f2 b2m cc =
  f1 (a2m f2 b2m cc) cc

instance MonadIO m => Monad (CallbackContT m) where
  (>>=) (CallbackContT cb_ma) a2_cb_mb = CallbackContT $ createCallbackB cb_ma a2_cb_mb
  return  =  pure

instance MonadTrans CallbackContT  where
  lift ma = CallbackContT (\f _cc -> join $ fmap f ma) -- should check canncel flag before?

instance MonadIO m => MonadIO (CallbackContT m) where
  liftIO ioa = lift $ liftIO ioa

instance (MonadState s m , MonadIO m) => MonadState s (CallbackContT m) where
  get  = lift get
  put s = lift $ put s

instance (MonadError e m, MonadIO m)  => MonadError e (CallbackContT m)  where
    throwError = lift . throwError
    catchError ma e2ma =
      let
        (CallbackContT fun) = ma
      in
        CallbackContT $ \cbFunc cc -> catchError (fun cbFunc cc) (\e -> runCallWithContext cc (e2ma e) cbFunc)

raceFunc :: MonadIO m => CallbackContT m a -> CallbackContT m b -> (Either a b -> m ()) -> m (m ())
raceFunc call1 call2 cb_func = do
  cc1 <- newCallContext
  cc2 <- newCallContext

  runCallWithContext cc1 call1 (\a-> cancelCall cc2 >> cb_func (Left a))
  runCallWithContext cc2 call2 (\b-> cancelCall cc1 >> cb_func (Right b))

  return $ cancelCall cc1 >> cancelCall cc2

raceCall :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m (Either a b)
raceCall cc1 cc2 = createCallbackCont (raceFunc cc1 cc2)

(>||<) :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m (Either a b)
(>||<) = raceCall
infixr 0 >||<

raceCall' :: MonadIO m => CallbackContT m a -> CallbackContT m a -> CallbackContT m a
raceCall' cc1 cc2 = do
  res <- raceCall cc1 cc2
  case res of
    Left a -> return a
    Right a -> return a

callWithOp :: MonadIO m => CallbackContT m a -> m () -> CallbackContT m a
callWithOp  call op =
  createCallbackCont (\cBack -> do
    cc <- runCall call cBack
    op
    return $ cancelCall cc)


(>|<) :: MonadIO m => CallbackContT m a -> CallbackContT m a -> CallbackContT m a
(>|<) = raceCall'
infixr 0 >|<

nestedFunc :: MonadIO m => CallbackContT m a -> CallbackContT m b -> (a -> m ()) -> m (m ())
nestedFunc outerCall innerCall cb_func = do
    ccInner <- newCallContext
    ccOuter <- newCallContext

    flNestedCall <- liftIO $ newTVarIO True
    runCallWithContext ccInner innerCall (\_-> liftIO $ atomically $ writeTVar flNestedCall False)
    runCallWithContext ccOuter outerCall (\a-> do
      cancelNestedCall flNestedCall ccInner
      cb_func a)
    return $ cancelNestedCall flNestedCall ccInner >> cancelCall ccOuter
  where
    cancelNestedCall flNestedCall cc = do
      isNestedCallInProgress <- liftIO $ atomically $ readTVar flNestedCall
      when isNestedCallInProgress $ cancelCall cc

nestedCall :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m a
nestedCall outerCall innerCall = createCallbackCont (nestedFunc outerCall innerCall)

(|:>) :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m a
(|:>) = nestedCall
infixr 0 |:>

within :: MonadIO m =>  CallbackContT m b -> CallbackContT m a -> CallbackContT m a
within = flip nestedCall

(<:|) :: MonadIO m =>  CallbackContT m b -> CallbackContT m a -> CallbackContT m a
(<:|) = within
infixr 0 <:|

type CallbackContTM m m1 a= CallbackContT m (m1 a)
altCalls :: (MonadIO m, MonadError e m,  Alternative m1) => CallbackContTM m m1 a ->CallbackContTM m m1 a -> CallbackContTM m m1 a
altCalls  = liftA2 (<|>)

{-
someCalls :: (MonadIO m, MonadError e m,  Alternative m1) => CallbackContTM m m1 a -> CallbackContTM m m1 [a]
someCalls  = fmap some

manyCalls :: (MonadIO m, MonadError e m,  Alternative m1) => CallbackContTM m m1 a -> CallbackContTM m m1 [a]
manyCalls  = fmap many
-}

data OneOf m a b = FirstCompleted a (CallbackContT m b) | SecondCompleted (CallbackContT m a) b
type OneOfState m a b = Maybe (Either (a->m()) (b->m()))


oneOfFunc :: MonadIO m => CallbackContT m a -> CallbackContT m b -> (OneOf m a b -> m ()) -> m (m ())
oneOfFunc call1 call2 cb_func = do
  cc1 <- newCallContext
  cc2 <- newCallContext

  oneOfState <- liftIO $ newTVarIO Nothing

  runCallWithContext cc1 call1 (\a-> handleOneCompletion cc2 (Left a)  oneOfState cb_func)
  runCallWithContext cc2 call2 (\b-> handleOneCompletion cc1 (Right b) oneOfState cb_func)

  return $ cancelCall cc1 >> cancelCall cc2

data CompletionResult m a b = OneDone (OneOf m a b) | FirstCallback a (a->m ()) | SecondCallback b (b->m()) | ErrorResult

handleOneCompletion :: MonadIO m => CallContext m -> Either a b -> TVar (OneOfState m a b) -> (OneOf m a b -> m ()) -> m ()
handleOneCompletion cc completedRes oneOfState cb_func  = do
  completionRes <- liftIO $ atomically $ do
    curState <- readTVar oneOfState
    case curState of
      Just (Left a_callback) -> case completedRes of
        Left a -> return $ FirstCallback a a_callback
        _ -> return  ErrorResult -- must not happen
      Just (Right b_callback) -> case completedRes of
        Right b -> return $ SecondCallback b b_callback
        _ -> return ErrorResult -- must not happen

      Nothing -> return $ OneDone $ case completedRes of
        Left a  -> FirstCompleted  a (createOneCallbackCont cc  oneOfState Right)
        Right b -> SecondCompleted (createOneCallbackCont cc  oneOfState Left) b

  case completionRes of
    ErrorResult -> return () -- log error
    OneDone oneCompleted -> cb_func oneCompleted
    FirstCallback  a aFunc -> aFunc a
    SecondCallback b bFunc -> bFunc b

createOneCallbackCont :: MonadIO m => CallContext m -> TVar (OneOfState m a b) -> ((c->m ()) -> Either (a->m ()) (b-> m ())) -> CallbackContT m c
createOneCallbackCont cc oneOfState funcSetter =
  CallbackContT $ \newCB newCC ->
    do
      liftIO $ atomically $ writeTVar oneOfState (Just (funcSetter newCB))
      setCancelFunc newCC (cancelCall cc)

oneOfCall :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m (OneOf m a b)
oneOfCall cc1 cc2 = createCallbackCont (oneOfFunc cc1 cc2)

-----------------------------------------------------------------------------------------------------------------------
data OneOf' m a b = FirstCompleted' a (CallbackContT m b) | SecondCompleted' b
type OneOfState' m b = Maybe (b->m())

oneOfFunc' :: MonadIO m => CallbackContT m a -> CallbackContT m b -> (OneOf' m a b -> m ()) -> m (m ())
oneOfFunc' call1 call2 cb_func = do
  cc1 <- newCallContext
  cc2 <- newCallContext

  -- this TVar will be used to store future callback if call2 completed later
  oneOfState <- liftIO $ newTVarIO Nothing
  runCallWithContext cc1 call1 (\a-> handleOneCompletion' cc2 (Left a)  oneOfState cb_func)
  runCallWithContext cc2 call2 (\b-> handleOneCompletion' cc1 (Right b) oneOfState cb_func)

  -- if cancelled, cancel both
  return $ cancelCall cc1 >> cancelCall cc2

data CompletionResult' m a b = OneDone' (OneOf' m a b) | FirstCallback' a (a->m ()) | SecondCallback' b (b->m()) | ErrorResult'

handleOneCompletion' :: MonadIO m => CallContext m -> Either a b -> TVar (OneOfState' m b) -> (OneOf' m a b -> m ()) -> m ()
handleOneCompletion' cc completedRes oneOfState cb_func  = do
  completionRes <- liftIO $ atomically $ do
    curState <- readTVar oneOfState
    case curState of
      Just b_callback -> case completedRes of
        Right b -> return $ SecondCallback' b b_callback
        _ -> return ErrorResult' -- must not happen

      Nothing -> return $ OneDone' $ case completedRes of
        Left a  -> FirstCompleted'  a (createOneCallbackCont' cc  oneOfState)
        Right b -> SecondCompleted' b

  case completionRes of
    ErrorResult' -> return () -- log error
    OneDone' oneCompleted -> cb_func oneCompleted
    FirstCallback'  a aFunc -> aFunc a
    SecondCallback' b bFunc -> bFunc b

createOneCallbackCont' :: MonadIO m => CallContext m -> TVar (OneOfState' m b) ->  CallbackContT m b
createOneCallbackCont' cc oneOfState =
  CallbackContT $ \newCB newCC ->
    do
      liftIO $ atomically $ writeTVar oneOfState (Just newCB)
      setCancelFunc newCC (cancelCall cc)

oneOfCall' :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m (OneOf' m a b)
oneOfCall' call1 call2 = createCallbackCont (oneOfFunc' call1 call2)

data OneOf3' m a b c = Res3 c | Res2 b (CallbackContT m c) | Res1 a (CallbackContT m (OneOf' m b c))
compactOneOf3' :: MonadIO m => OneOf' m (OneOf' m a b) c -> OneOf3' m a b c
compactOneOf3' oneOfoneOf = case oneOfoneOf of
  SecondCompleted' cVal -> Res3 cVal
  FirstCompleted' (SecondCompleted' bVal) cCB -> Res2 bVal cCB
  FirstCompleted' (FirstCompleted' aVal bCB) cCB -> Res1 aVal (oneOfCall' bCB cCB)

oneOf3Call' :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m c ->  CallbackContT m (OneOf3' m a b c)
oneOf3Call' call1 call2 call3 = fmap compactOneOf3' (oneOfCall' (oneOfCall' call1 call2) call3)

-----------------------------------------------------------------------------------------------------------
infiniteCall :: MonadIO m => CallbackContT m Never
infiniteCall = createCallbackCont (const $ (return.return) ())

-----------------------------------------------------------------------------------------------------------
newtype ResourceC m res = ResourceC {allocRes :: m (res, CallContext m)}
newtype AllocatedResourceC m res = AllocatedResourceC {getRes :: (res, CallContext m)}

newRes :: (MonadIO m, MonadError e m) => m (res, m ()) -> ResourceC m res
newRes resAndRelease = ResourceC $ do
  (res, rlsFunc) <- resAndRelease
  cc <- newCallContext
  setCancelFunc cc rlsFunc
  return (res, cc)


{-
withResource' :: (MonadIO m, MonadError e m) => ResourceC m res -> (res->CallbackContT m a) -> CallbackContT m a
withResource' resC callbackFromRes = do
  (res, releaseResFunc) <- lift (allocRes resC)
  a <- catchError (callbackFromRes res) (\e -> lift releaseResFunc >> throwError e)
  lift releaseResFunc
  return a
-}

--resource is created inside and is released upon callback completion
--if canncelled from outside then 1) resource is released 2)callback cancelled
withResource :: (MonadIO m, MonadError e m) => ResourceC m res -> (res->CallbackContT m a) -> CallbackContT m a
withResource resC callbackFromRes = createCallbackCont (\cbFunc -> do
  (res, resCC) <- allocRes resC
  let cbWithRes = callbackFromRes res
  cc <- runCall cbWithRes (\a-> cancelCall resCC >> cbFunc a)
  return $ cancelCall cc >> cancelCall resCC)

resourceCall :: (MonadIO m, MonadError e m) => ResourceC m a -> CallbackContT m Never
resourceCall resC = createCallbackCont (\_ -> do
  (_res, ccRes) <- allocRes resC
  return $ cancelCall ccRes)

--resource is created inside and but is not released upon callback completion
--if canncelled from outside then 1) resource is released 2)callback cancelled
withResourceOut :: (MonadIO m, MonadError e m) => ResourceC m res -> (res->CallbackContT m a) -> CallbackContT m (a, AllocatedResourceC m res)
withResourceOut resC callbackFromRes = createCallbackCont (\cbFunc -> do
  allocatedRes@(res, ccRes) <- allocRes resC
  let cbWithRes = callbackFromRes res
  cc <- runCall cbWithRes (\a-> cbFunc (a, AllocatedResourceC allocatedRes))
  return $ cancelCall cc >> cancelCall ccRes)

--allocatede resource is coming  outside and is not released upon callback completion
--if canncelled from outside then 1) resource is released 2)callback cancelled
withResourceInOut :: (MonadIO m, MonadError e m) => AllocatedResourceC m res -> (res->CallbackContT m a) -> CallbackContT m (a, AllocatedResourceC m res)
withResourceInOut allocatedResC callbackFromRes = createCallbackCont (\cbFunc -> do
  let (res, ccRes) = getRes allocatedResC
  let cbWithRes = callbackFromRes res
  cc <- runCall cbWithRes (\a-> cbFunc (a, allocatedResC))
  return $ cancelCall cc >> cancelCall ccRes)

--allocatede resource is coming  outside and is released upon callback completion
--if canncelled from outside then 1) resource is released 2)callback cancelled
withResourceIn :: (MonadIO m, MonadError e m) => AllocatedResourceC m res -> (res->CallbackContT m a) -> CallbackContT m a
withResourceIn allocatedResC callbackFromRes = createCallbackCont (\cbFunc -> do
  let (res, ccRes) = getRes allocatedResC
  let cbWithRes = callbackFromRes res
  cc <- runCall cbWithRes (\a-> cancelCall ccRes >> cbFunc a)
  return $ cancelCall cc >> cancelCall ccRes)

withResource_ :: (MonadIO m, MonadError e m) => ResourceC m () -> CallbackContT m a -> CallbackContT m a
withResource_ resC callback = withResource resC (const callback)

(>:>) :: (MonadIO m, MonadError e m) => ResourceC m () -> CallbackContT m a -> CallbackContT m a
(>:>) = withResource_

-- like (>:>) but dont release on cancelation, should be used when resource is released internatly on cancelation
(>|>) :: (MonadIO m, MonadError e m) => ResourceC m () -> CallbackContT m a -> CallbackContT m a
(>|>) resC callbackCont = createCallbackCont (\cbFunc -> do
  ((), ccRes) <- allocRes resC
  cc <- runCall callbackCont (\a-> cancelCall ccRes >> cbFunc a)
  return $ cancelCall cc)


bothCalls :: MonadIO m => CallbackContT m a -> CallbackContT m b -> CallbackContT m (a,b)
bothCalls call1 call2 = do
  oneOfRes <- oneOfCall call1 call2
  case oneOfRes of
    FirstCompleted a cbb -> do {b <- cbb; return (a,b)}
    SecondCompleted cba b -> do {a <- cba; return (a,b)}

manyCalls :: MonadIO m => [CallbackContT m a] -> CallbackContT m [a]
manyCalls [] = return []
manyCalls [call] = (: []) <$> call
manyCalls (call:calls) = (\(a,al)->a:al) <$> bothCalls call (manyCalls calls)

------------------------------------------------------------------------------------------------------------
