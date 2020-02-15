{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module CallbackEventDistrTest (callbackEventDisrTest) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Control.Lens
import Data.Text

import Utils
import ARICore
import ARITestEnv
import FlowTools hiding (evDistr)

emptyText :: Text
emptyText = ""

data E1 = E1 deriving (Show,Eq)
data E2 = E2 deriving (Show,Eq)
data Event = Event1 E1 | Event2 E2 deriving (Show,Eq)

data StateWithEvDistr =  StateWithEvDistr {_evDistr ::  EventDistributor Event (FlowContextM StateWithEvDistr) StateWithEvDistr, _textState :: Text}
makeLenses ''StateWithEvDistr

dLens :: EventDistributorLens Event (FlowContextM StateWithEvDistr) StateWithEvDistr
dLens = evDistr

initState :: StateWithEvDistr
initState = StateWithEvDistr newEventDistributor ""

appendToState' :: (Show ev) => ev -> FlowContext StateWithEvDistr ()
appendToState' ev = textState %= (\curTxt -> curTxt <> (show ev))

appendToState :: (Show ev) => ev -> FlowCallback StateWithEvDistr ()
appendToState = lift . appendToState'

isEv1 :: Event -> Maybe E1
isEv1 (Event1 e1) = Just e1
isEv1 _ = Nothing

isEv2 :: Event -> Maybe E2
isEv2 (Event2 e2) = Just e2
isEv2 _ = Nothing

waitForEv :: (Event-> Maybe e) -> FlowCallback StateWithEvDistr e
waitForEv = monitorCallbackWithTrans (evDistr)  -- transReq

waitForE1 :: FlowCallback StateWithEvDistr E1
waitForE1 = waitForEv isEv1

waitForE2 :: FlowCallback StateWithEvDistr E2
waitForE2 = waitForEv isEv2

fireE1 :: FlowCallback StateWithEvDistr ()
fireE1 = lift $ notifyEvent evDistr (Event1 E1)

fireE2 :: FlowCallback StateWithEvDistr ()
fireE2 = lift $ notifyEvent evDistr (Event2 E2)

printKeys :: Text -> FlowCallback StateWithEvDistr ()
printKeys text = do
  keys <- lift (monitors evDistr)
  liftIO $ putStrLn (text <> ":" <>show keys)

call1 :: FlowCallback StateWithEvDistr (Either E1 ())
call1 = do
  printKeys "call1 (init)"

  res <- (waitForE1 >||< timeCallback 1000000) |:>
    (timeCallback 1000 >> printKeys "call1 (waiting)" >> fireE1)
  liftIO $ putStrLn (("call1Res:"::Text) <> show res)
  printKeys "call1 (completed)"
  appendToState res
  return res

call2 :: FlowCallback StateWithEvDistr ()
call2 = do
  let waitE1E2 = waitForE1 >||< waitForE2
  _cc0 <- lift $ runCall (timeCallback 1000 >||< waitE1E2) appendToState'
  liftIO $ putStrLn ("going to waitForE2"::Text)
  _cc1 <- lift $ runCall waitForE2 appendToState'
  liftIO $ putStrLn ("waitingForE2"::Text)
  _cc2 <- lift $ runCall waitE1E2 appendToState'
  _cc3 <- lift $ runCall waitE1E2 appendToState'
  timeCallback 1000000
  fireE2
  return ()

call3 :: FlowCallback StateWithEvDistr ()
call3 = do
  let waitE1E2 = waitForE1 >||< waitForE2
  let waitE1E2Call = void $ runCall waitE1E2 appendToState'
  _cc0 <- lift $ runCall (timeCallback 1000 >||< waitE1E2) (\res -> (appendToState' res) >> waitE1E2Call >> waitE1E2Call)
  liftIO $ putStrLn ("going to waitForE2"::Text)
  _cc1 <- lift $ runCall waitForE2 appendToState'
  liftIO $ putStrLn ("waitingForE2"::Text)
  timeCallback 1000000

  keys1 <- lift (monitors evDistr)
  liftIO $ putStrLn (("keys1"::Text) <> show keys1)
  fireE2
  keys2 <- lift (monitors evDistr)
  liftIO $ putStrLn (("keys2"::Text) <> show keys2)

  return ()

callbackEventDisrTest :: ARIEnv -> IO ()
callbackEventDisrTest ariEnv= hspec $ do
  describe "Callback Event distr test" $ do
    it "wait and fire with single event" $ do
      let flow1 = runCall call1 (\_res-> exitFlow)
      (res, st) <- runFlowWithContext flow1 ariEnv initState
      (_textState st) `shouldBe` "Left E1"
      res `shouldBe` Right ()
    it "wait for both and cancel then wait for one and both twice" $ do
      let flow2 = runCall call3 (\_res-> exitFlow)
      (res, st) <- runFlowWithContext flow2 ariEnv initState
      (_textState st) `shouldBe` "Left ()E2Right E2Right E2"
      res `shouldBe` Right ()
    it "wait for both and cancel ,then wait for one and both twice (concurrently with 1st wait)" $ do
      let flow2 = runCall call2 (\_res-> exitFlow)
      (res, st) <- runFlowWithContext flow2 ariEnv initState
      (_textState st) `shouldBe` "Left ()E2Right E2Right E2"
      res `shouldBe` Right ()
