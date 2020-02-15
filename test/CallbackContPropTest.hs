module CallbackContPropTest (callbackContPropTest) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Exception (evaluate)
import Control.Monad

import Utils
import ARICore
import ARITestEnv
import FlowTools

emptyText :: Text
emptyText = ""

echoCallBack :: Int -> FlowCallback Int Int
echoCallBack i = return i

callbackContPropTest :: ARIEnv -> IO ()
callbackContPropTest ariEnv = do
  putStrLn ("callbackContPropTest ----------------------------------------------------------------------------"::Text)
  putStrLn ("prop_echo_callback"::Text)
  quickCheck (prop_echo_callback ariEnv)
  putStrLn ("prop_fmap_echo_callback"::Text)
  quickCheck (prop_fmap_echo_callback ariEnv)
  putStrLn ("callbackContPropTest ----------------------------------------------------------------------------"::Text)

prop_echo_callback :: ARIEnv -> Int -> Property
prop_echo_callback ariEnv x = monadicIO $ do
  (res,s) <- run $ runFlowWithContext (runCall (echoCallBack x) setStateAndExit) ariEnv 0
  pre (res == Right ())
  assert (s==x)

prop_fmap_echo_callback :: ARIEnv -> Int -> Property
prop_fmap_echo_callback ariEnv x = monadicIO $ do
  (res,s) <- run $ runFlowWithContext (runCall (fmap (+10) (echoCallBack x)) setStateAndExit) ariEnv 0
  pre (res == Right ())
  assert (s==(x+10))
