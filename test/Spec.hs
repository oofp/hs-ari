import Protolude
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad

import Utils
import ARICore
import FlowTools

import ARITestEnv
import BasicTest
import CallbackContTest
import CallbackContPropTest
import ARIDataTest
import EventDistrTest
import DTMFTransTest
import CallbackEventDistrTest

emptyText :: Text
emptyText = ""

main :: IO ()
main = do
  setupLog
  infoM rootLog "Starting hARI"

  ariEnv <- newAriEnv

  let helloWorldFlow = lift (put "Hello World")  >>exitFlow
  (res, st) <- runFlowWithContext helloWorldFlow ariEnv emptyText
  print res
  print st

  callbackEventDisrTest ariEnv
  basicTest ariEnv
  callbackContTest ariEnv
  callbackContPropTest ariEnv
  interFlowTest ariEnv
  ariDataTest
  eventDisrTest ariEnv
  dtmfTransTest


basicTest1 :: ARIEnv -> IO ()
basicTest1 ariEnv= hspec $ do
  describe "Basic flow test" $ do
    it "test empty scenario" $ do
      (res, st) <- runFlowWithContext exitFlow ariEnv emptyText
      st `shouldBe` ""
      -- res `shouldBe` (Right ())
    it "test set state scenario" $ do
      (res, st) <- runFlowWithContext (setState "Hello" >> exitFlow) ariEnv emptyText
      st `shouldBe` ("Hello"::Text)
      -- res `shouldBe` (Right ())
    it "test error scenario" $ do
      (res, st) <- runFlowWithContext (setState "HelloError" >> throwError (GenericError "MyError") >> exitFlow) ariEnv emptyText
      st `shouldBe` ("HelloError"::Text)
      res `shouldBe` (Left $ GenericError "MyError")
