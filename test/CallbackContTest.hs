module CallbackContTest
  ( callbackContTest
  , interFlowTest
  ) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Exception (evaluate)
import Control.Monad
import Control.Concurrent.STM.TVar
import Prelude (String)
import Control.Concurrent.Thread.Delay

import Utils
import ARICore
import ARITestEnv
import FlowTools

emptyText :: Text
emptyText = ""

helloCallback :: FlowCallback Text Text
helloCallback = return "Hello"

echoCallBack :: Int -> FlowCallback Int Int
echoCallBack i = return i

returnList :: FlowCallback Text [Text]
returnList = return ["1","22","333"]

-- type FlowResult = Either FlowError ()

setStateUntil :: FlowCallback [Int] (Maybe Text)
setStateUntil = do
  curList <- lift $ lift get
  if length curList >20
    then return Nothing
    else
      do
        liftIO $ putStrLn (("curList:"::String) ++ show curList)
        lift $ lift $ put (curList <> [1])
        return $ Just ""


callbackContTest :: ARIEnv -> IO ()
callbackContTest ariEnv= hspec $ do
  describe "CallbackT test" $ do
    it "test return" $ do
      let runHelloCallBack  = runCall helloCallback setStateAndExit
      (res, st) <- runFlowWithContext runHelloCallBack ariEnv emptyText
      st `shouldBe` "Hello"
      res `shouldBe` Right ()
    it "test fmap" $ do
      let cbFlowContext  = runCall (fmap mconcat returnList) setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv emptyText
      st `shouldBe` "122333"
      res `shouldBe` Right ()
    it "test alt <|> '1' '2' " $ do
      let cbFlowContext  = runCall (altCalls (return $ Just ("1"::Text)) (return $ Just ("2"::Text))) appendStateFAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv (Just ("0_"::Text))
      st `shouldBe` Just "0_1"
      res `shouldBe` Right ()
    it "test alt <|> Nothing '2' " $ do
      let cbFlowContext  = runCall (altCalls (return Nothing) (return $ Just ("2"::Text))) appendStateFAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv (Just ("0_"::Text))
      st `shouldBe` Just "0_2"
      res `shouldBe` Right ()
    it "test alt <|> Nothing Nothing " $ do
      let cbFlowContext  = runCall (altCalls (return Nothing) (return Nothing)) appendStateFAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv (Just ("0_"::Text))
      st `shouldBe` Nothing
      res `shouldBe` Right ()
    {-
    it "test some" $ do
      let cbFlowContext  = runCall (someCalls setStateUntil) (const exitFlow)
      (res, st) <- runFlowWithContext cbFlowContext ariEnv []
      st `shouldBe` replicate 20 1
      res `shouldBe` Right ()
    it "test many" $ do
      let cbFlowContext  = runCall (manyCalls setStateUntil) (const exitFlow)
      (res, st) <- runFlowWithContext cbFlowContext ariEnv []
      st `shouldBe` replicate 20 1
      res `shouldBe` Right ()
    -}
    it "raceCalls" $ do
      let t1=timeCallback 1000000 >> return ("t1"::Text)
      let t2=timeCallback 2000000 >> return ("t2"::Text)
      let tCB = raceCall t1 t2
      let cbFlowContext  = runCall tCB setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv (Right emptyText)
      st `shouldBe` Left "t1"
      res `shouldBe` Right ()
    it "raceCalls2" $ do
      let t1=timeCallback 1000000 >> appendStateCB ("t1"::Text) >> timeCallback 500000 >> appendStateCB ("t2"::Text) >> timeCallback 1000000 >> appendStateCB ("t3"::Text)
      let t2=timeCallback 2000000 >> appendStateCB ("t4"::Text)
      let tCB = raceCall t1 t2
      let cbFlowContext  = runCall tCB (const exitFlow)
      (res, st) <- runFlowWithContext cbFlowContext ariEnv emptyText
      st `shouldBe` "t1t2t4"
      res `shouldBe` Right ()
    it "oneOf test " $ do
      let t1a = timeCallback 1000000 >> appendStateCB ("t1a"::Text)
      let t1b = timeCallback 1500000 >> appendStateCB ("t1b"::Text)
      let t1 = t1a >> t1b
      let t2 = timeCallback 2000000 >> appendStateCB ("t2"::Text)
      let t12 = oneOfCall t1 t2
      let waitForBothTimers = do
                              t12Res <- t12
                              _ <- case t12Res of
                                FirstCompleted  _ t2going -> appendStateCB "_FirstCompleted"  >>  t2going
                                SecondCompleted t1going _  -> appendStateCB "_SecondCompleted" >>  t1going
                              appendStateCB "_Completed both"

      let runBothTimersContext = runCall waitForBothTimers  (const exitFlow)
      (res, st) <- runFlowWithContext runBothTimersContext ariEnv emptyText
      st `shouldBe` "t1at2_SecondCompletedt1b_Completed both"
      res `shouldBe` Right ()
    it "oneOf test & race" $ do
      let t1a = timeCallback 1000000 >> appendStateCB ("t1a"::Text)
      let t1b = timeCallback 1500000 >> appendStateCB ("t1b"::Text)
      let t1 = t1a >> t1b
      let t2 = timeCallback 2000000 >> appendStateCB ("t2"::Text)
      let t12 = oneOfCall t1 t2
      let waitForBothTimers = do
                              t12Res <- t12
                              _ <- case t12Res of
                                FirstCompleted  _ t2going -> appendStateCB "_FirstCompleted"  >>  t2going
                                SecondCompleted t1going _  -> appendStateCB "_SecondCompleted" >>  t1going
                              appendStateCB "_Completed both"
      let t3 =timeCallback 2200000 >> appendStateCB ("t3"::Text)
      let t123=raceCall waitForBothTimers t3
      let runBothTimersContext = runCall t123  (const exitFlow)
      (res, st) <- runFlowWithContext runBothTimersContext ariEnv emptyText
      st `shouldBe` "t1at2_SecondCompletedt3"
      res `shouldBe` Right ()
    it "oneOf' test first complete first" $ do
      let t1 = timeCallback 1000000 >> appendStateCB ("t1"::Text)
      let t2 = timeCallback 1500000 >> appendStateCB ("t2"::Text)
      let t12 = oneOfCall' t1 t2
      let waitForBothTimers = do
                              t12Res <- t12
                              _ <- case t12Res of
                                FirstCompleted'  _ t2going -> appendStateCB "_FirstCompleted"  >>  t2going >> appendStateCB "_t2Completed"
                                SecondCompleted' _t2res   -> appendStateCB "_SecondCompleted"
                              appendStateCB "_Completed both"

      let runBothTimersContext = runCall waitForBothTimers  (const exitFlow)
      (res, st) <- runFlowWithContext runBothTimersContext ariEnv emptyText
      st `shouldBe` "t1_FirstCompletedt2_t2Completed_Completed both"
      res `shouldBe` Right ()
    it "oneOf' test second complete first" $ do
      let t1 = timeCallback 1500000 >> appendStateCB ("t1"::Text)
      let t2 = timeCallback 1000000 >> appendStateCB ("t2"::Text)
      let t12 = oneOfCall' t1 t2
      let waitForBothTimers = do
                              t12Res <- t12
                              _ <- case t12Res of
                                FirstCompleted'  _ t2going -> appendStateCB "_FirstCompleted"  >>  t2going >> appendStateCB "_t2Completed"
                                SecondCompleted' _t2res   -> appendStateCB "_SecondCompleted"
                              appendStateCB "_Completed both"

      let runBothTimersContext = runCall waitForBothTimers  (const exitFlow)
      (res, st) <- runFlowWithContext runBothTimersContext ariEnv emptyText
      st `shouldBe` "t2_SecondCompleted_Completed both"
      res `shouldBe` Right ()

interFlowTest :: ARIEnv -> IO ()
interFlowTest ariEnv= hspec $ do
  describe "Inter Flows/Calls" $ do
    it "Invoke flow/call" $ do
      let t1=timeCallback 1000000 >> return ("t1"::Text)
      let invokeCall = evalCallComp t1 (return ()) emptyText
      let cbFlowContext  = runCall invokeCall setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv emptyText
      st `shouldBe` "t1"
      res `shouldBe` Right ()
    it "Invoke flow/call (with TVar)" $ do
      textTVar <- newTVarIO emptyText
      let writeTVarCall = liftIO $ atomically $ writeTVar textTVar "t1 completed"
      let t1=timeCallback 1000000 >> writeTVarCall >> return ("t1"::Text)
      let invokeCall = evalCallComp t1 (return ()) emptyText
      let cbFlowContext  = runCall invokeCall setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv emptyText
      tvarVal <- liftIO $ readTVarIO textTVar
      st `shouldBe` "t1"
      tvarVal `shouldBe` "t1 completed"
      res `shouldBe` Right ()
    it "Race of invoked flow/call with local timer; local complete first" $ do
      textTVar <- newTVarIO emptyText
      let writeTVarCall1 = appendToTVar textTVar "t1 completed"
      let writeTVarCall0 = appendToTVar textTVar "t0 completed"
      let t1=timeCallback 1000000 >> writeTVarCall1 >> return ("t1"::Text)
      let t0=timeCallback 500000 >> writeTVarCall0 >> return ("t0"::Text)
      let invokeCall = evalCallComp t1 (return ()) emptyText
      let tCB = raceCall t0 invokeCall
      let cbFlowContext  = runCall tCB setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv (Right emptyText)
      tvarVal <- liftIO $ readTVarIO textTVar
      st `shouldBe` Left "t0"
      res `shouldBe` Right ()
      tvarVal `shouldBe` "t0 completed"
    it "Race of invoked flow/call with local timer; other flow complete first" $ do
      textTVar <- newTVarIO emptyText
      let writeTVarCall1 = appendToTVar textTVar "t1 completed"
      let writeTVarCall0 = appendToTVar textTVar "t0 completed"
      let t1=timeCallback 500000 >> writeTVarCall1 >> return ("t1"::Text)
      let t0=timeCallback 1000000 >> writeTVarCall0 >> return ("t0"::Text)
      let invokeCall = evalCallComp t1 (return ()) emptyText
      let tCB = raceCall t0 invokeCall
      let cbFlowContext  = runCall tCB setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv (Right emptyText)
      tvarVal <- liftIO $ readTVarIO textTVar
      st `shouldBe` Right "t1"
      res `shouldBe` Right ()
      tvarVal `shouldBe` "t1 completed"
    it "runCallComp" $ do
      let t1=timeCallback 1000000 >> lift (lift $ put (1::Int)) >> return ("t1"::Text)
      let invokeCall = runCallComp t1 (return ()) 0
      let cbFlowContext  = runCall invokeCall setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv ("",0)
      st `shouldBe` ("t1",1)
      res `shouldBe` Right ()
    it "execCallComp" $ do
      let t1=timeCallback 1000000 >> lift (lift $ put (1::Int)) >> return ("t1"::Text)
      let invokeCall = execCallComp t1 (return ()) 0
      let cbFlowContext  = runCall invokeCall setStateAndExit
      (res, st) <- runFlowWithContext cbFlowContext ariEnv 0
      st `shouldBe` 1
      res `shouldBe` Right ()
    it "State preserved on cancelation test" $ do
      let t1=lift (lift $ put (1::Int)) >> timeCallback 1000000 >> lift (lift $ put (2::Int)) >> return ("t1"::Text)
      let cbFlowContext  = runAndCancel t1 >>  exitFlow
      (res, st) <- runFlowWithContext cbFlowContext ariEnv 0
      st `shouldBe` 1
      res `shouldBe` Right ()
    it "onCancel test" $ do
      intTVar <- newTVarIO Nothing
      let t1=lift (lift $ put (1::Int)) >> timeCallback 1000000 >> lift (lift $ put (2::Int)) >> return ("t1"::Text)
      let t2 = onCancel  (liftIO . atomically . writeTVar intTVar .Just) t1
      let cbFlowContext  = runAndCancel t2 >>  exitFlow
      (res, st) <- runFlowWithContext cbFlowContext ariEnv 0
      st `shouldBe` 1
      res `shouldBe` Right ()
      tvarVal <- liftIO $ readTVarIO intTVar
      tvarVal `shouldBe` Just 1
    it "onCancelAsync test" $ do
      intTVar <- newTVarIO Nothing
      let t1=lift (lift $ put (1::Int)) >> timeCallback 1000000 >> lift (lift $ put (2::Int)) >> return ("t1"::Text)
      let t2 = onCancelAsync  (\s-> (delay 100000) >> (atomically $ writeTVar intTVar (Just s))) t1
      --let t2 = onCancelAsync  (atomically . writeTVar intTVar .Just) t1
      let cbFlowContext  = runAndCancel t2 >>  exitFlow
      (res, st) <- runFlowWithContext cbFlowContext ariEnv 0
      st `shouldBe` 1
      res `shouldBe` Right ()
      liftIO $ delay 500000
      tvarVal <- liftIO $ readTVarIO intTVar
      tvarVal `shouldBe` Just 1
