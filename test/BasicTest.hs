module BasicTest (basicTest) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad

import Utils
import ARICore
import ARITestEnv

emptyText :: Text
emptyText = ""

-- type FlowResult = Either FlowError ()

basicTest :: ARIEnv -> IO ()
basicTest ariEnv= hspec $ do
  describe "Basic flow test" $ do
    it "test empty scenario" $ do
      (res, st) <- runFlowWithContext exitFlow ariEnv emptyText
      st `shouldBe` ""
      res `shouldBe` (Right ())
    it "test set state scenario" $ do
      (res, st) <- runFlowWithContext (setState "Hello" >> exitFlow) ariEnv emptyText
      st `shouldBe` ("Hello"::Text)
      res `shouldBe` (Right ())
    it "test error scenario" $ do
      (res, st) <- runFlowWithContext (setState "HelloError" >> throwError (GenericError "MyError") >> exitFlow) ariEnv emptyText
      st `shouldBe` ("HelloError"::Text)
      res `shouldBe` (Left $ GenericError "MyError")
