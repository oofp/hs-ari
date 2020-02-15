{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventDistrTest (eventDisrTest) where

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

data StateWithEvDistr =  StateWithEvDistr {_evDistr ::  EventDistributor Text (FlowContextM StateWithEvDistr) StateWithEvDistr, _textState :: Text}
makeLenses ''StateWithEvDistr

dLens :: EventDistributorLens Text (FlowContextM StateWithEvDistr) StateWithEvDistr
dLens = evDistr

initState :: StateWithEvDistr
initState = StateWithEvDistr newEventDistributor ""

monitorHandlerOnce :: EventID -> Text -> FlowContext StateWithEvDistr Bool
monitorHandlerOnce _evID txt = do
  textState %= (\curTxt -> curTxt <> txt)
  return False

monitorHandlerMany :: EventID -> Text -> FlowContext StateWithEvDistr Bool
monitorHandlerMany _evID txt = do
  textState %= (\curTxt -> curTxt <> txt)
  return True

monitorHandlerRecurs :: Int -> EventID -> Text -> FlowContext StateWithEvDistr Bool
monitorHandlerRecurs counter _evID _txt = do
  -- liftIO $ putStrLn (("monitorHandlerRecurs before entered:"::Text) <> (show counter))
  textState %= (\curTxt -> curTxt <> show counter)
  when   (counter>0) (void $ addMonitor evDistr (EventMonitor (const True) (monitorHandlerRecurs (counter-1))))
  -- liftIO $ putStrLn (("monitorHandlerRecurs before exit:"::Text) <> (show counter))
  return False

monitorHandlerWithNotify :: Int -> EventID -> Text -> FlowContext StateWithEvDistr Bool
monitorHandlerWithNotify counter _evID _txt = do
  -- liftIO $ putStrLn (("monitorHandlerRecurs before entered:"::Text) <> (show counter))
  textState %= (\curTxt -> curTxt <> show counter)
  when   (counter>0) (void $ addMonitor evDistr (EventMonitor (const True) (monitorHandlerRecurs (counter-1))))
  notifyEvent evDistr (show counter)
  -- liftIO $ putStrLn (("monitorHandlerRecurs before exit:"::Text) <> (show counter))
  return False

monitorHandlerUntil :: Text -> EventID -> Text -> FlowContext StateWithEvDistr Bool
monitorHandlerUntil untilText _evID txt = do
  textState %= (\curTxt -> curTxt <> txt)
  return (untilText /= txt)

monitorInt :: EventID -> Int -> FlowContext StateWithEvDistr Bool
monitorInt _evID iVal = do
  textState %= (\curTxt -> curTxt <> (show iVal))
  return True

monitorAll :: EventMonitor Text (FlowContextM StateWithEvDistr)
monitorAll = EventMonitor (const True) monitorHandlerOnce

flow1 :: FlowContext StateWithEvDistr ()
flow1 = do
  void $ addMonitor evDistr (EventMonitor (const True) monitorHandlerOnce)
  notifyEvent evDistr "Hello"
  exitFlow

flow2 :: FlowContext StateWithEvDistr ()
flow2 = do
  void $ addMonitor evDistr (EventMonitor (const True) monitorHandlerOnce)
  notifyEvent evDistr "Notif1"
  notifyEvent evDistr "Notif2"
  exitFlow

flow3 :: FlowContext StateWithEvDistr ()
flow3 = do
  void $ addMonitor evDistr (EventMonitor (const True) monitorHandlerMany)
  notifyEvent evDistr "Notif1"
  notifyEvent evDistr "Notif2"
  exitFlow

flow4 :: FlowContext StateWithEvDistr ()
flow4 = do
  void $ addMonitor evDistr (EventMonitor (const True) monitorHandlerOnce)
  void $ addMonitor evDistr (EventMonitor (const True) monitorHandlerMany)
  notifyEvent evDistr "Notif1"
  notifyEvent evDistr "Notif2"
  exitFlow

flow5 :: FlowContext StateWithEvDistr ()
flow5 = do
  void $ addMonitor evDistr (EventMonitor (const True) (monitorHandlerRecurs 9))
  -- replicateM_ 9 $ submitTask ((notifyEvent evDistr "Notif1") >> (liftIO $ putStrLn ("just notified"::Text)))
  replicateM_ 9 (notifyEvent evDistr "Notif1")
  exitFlow

flow6 :: FlowContext StateWithEvDistr ()
flow6 = do
  void $ addMonitor evDistr (EventMonitor (const True) (monitorHandlerRecurs 9))
  -- replicateM_ 9 $ submitTask ((notifyEvent evDistr "Notif1") >> (liftIO $ putStrLn ("just notified"::Text)))
  replicateM_ 9 (notifyEvent evDistr "Notif1")
  exitFlow

flow7 :: FlowContext StateWithEvDistr ()
flow7 = do
  liftIO $ putStrLn ("Entered flow7"::Text)

  let list10 :: [Text]
      list10 = fmap show $ enumFromTo 0 (10::Int)
      listEven =  fmap show $ enumFromThenTo 0 2 (10::Int)

  void $ addMonitor evDistr (EventMonitor (`elem` listEven) monitorHandlerMany)
  mapM_ (notifyEvent evDistr) list10
  exitFlow

flow8 :: FlowContext StateWithEvDistr ()
flow8 = do
  liftIO $ putStrLn ("Entered flow8"::Text)
  let list10 :: [Text]
      list10 = fmap show $ enumFromTo 0 (10::Int)
      listEven =  fmap show $ enumFromThenTo 0 2 (10::Int)
      listAbove5 =  fmap show $ enumFromTo 5 (10::Int)

  void $ addMonitor evDistr (EventMonitor (`elem` listEven) monitorHandlerMany)
  void $ addMonitor evDistr (EventMonitor (`elem` listAbove5) monitorHandlerMany)
  void $ addMonitor evDistr (EventMonitor (const True) monitorHandlerMany)
  mapM_ (notifyEvent evDistr) list10
  exitFlow

flow9 :: FlowContext StateWithEvDistr ()
flow9 = do
  liftIO $ putStrLn ("Entered flow9"::Text)
  let list10 :: [Text]
      list10 = fmap show $ enumFromTo 0 (10::Int)
      listEven =  fmap show $ enumFromThenTo 0 2 (10::Int)
      listAbove5 =  fmap show $ enumFromTo 5 (10::Int)

  void $ addMonitor evDistr (EventMonitor (`elem` listEven) monitorHandlerMany)
  void $ addMonitor evDistr (EventMonitor (`elem` listAbove5) monitorHandlerMany)
  void $ addMonitor evDistr (EventMonitor (const True) (monitorHandlerUntil "4"))
  mapM_ (notifyEvent evDistr) list10
  exitFlow

doubleIfEven :: Text -> Maybe Int
doubleIfEven text = do
  let  iValMaybe::Maybe Int
       iValMaybe = readMaybe (unpack text)
  iVal <- iValMaybe
  if (iVal `mod` 2) == 0
    then return (iVal*2)
    else Nothing

flow10 :: FlowContext StateWithEvDistr ()
flow10 = do
  liftIO $ putStrLn ("Entered flow10"::Text)
  let list10 :: [Text]
      list10 = fmap show $ enumFromTo 0 (10::Int)

  void $ addMonitorWithTrans evDistr doubleIfEven monitorInt
  mapM_ (notifyEvent evDistr) list10
  exitFlow


eventDisrTest :: ARIEnv -> IO ()
eventDisrTest ariEnv= hspec $ do
  describe "Event distr test" $ do
    it "create simple event distr with single monitor-single event" $ do
      (res, st) <- runFlowWithContext flow1 ariEnv initState
      (_textState st) `shouldBe` "Hello"
      res `shouldBe` Right ()
    it "create simple event distr with single monitor-multiple events-one notification" $ do
      (res, st) <- runFlowWithContext flow2 ariEnv initState
      (_textState st) `shouldBe` "Notif1"
      res `shouldBe` Right ()
    it "create simple event distr with single monitor-two events-two notifications" $ do
      (res, st) <- runFlowWithContext flow3 ariEnv initState
      (_textState st) `shouldBe` "Notif1Notif2"
      res `shouldBe` Right ()
    it "create simple event distr with two monitor-two events-three notifications" $ do
      (res, st) <- runFlowWithContext flow4 ariEnv initState
      (_textState st) `shouldBe` "Notif1Notif1Notif2"
      res `shouldBe` Right ()
    it "single recirsive monitor" $ do
      (res, st) <- runFlowWithContext flow5 ariEnv initState
      (_textState st) `shouldBe` "987654321"
      res `shouldBe` Right ()
    it "single recirsive monitor wth notify" $ do
      (res, st) <- runFlowWithContext flow6 ariEnv initState
      (_textState st) `shouldBe` "987654321"
      res `shouldBe` Right ()
    it "monitor with filter" $ do
      (res, st) <- runFlowWithContext flow7 ariEnv initState
      (_textState st) `shouldBe` "0246810"
      res `shouldBe` Right ()
    it "two monitors with filters and one monitor no filter" $ do
      (res, st) <- runFlowWithContext flow8 ariEnv initState
      (_textState st) `shouldBe` "00122344556667788899101010"
      res `shouldBe` Right ()
    it "two monitors with filters and one monitor unti" $ do
      (res, st) <- runFlowWithContext flow9 ariEnv initState
      (_textState st) `shouldBe` "0012234456678891010"
      res `shouldBe` Right ()
    it "monotor with trans" $ do
      (res, st) <- runFlowWithContext flow10 ariEnv initState
      (_textState st) `shouldBe` "048121620"
      res `shouldBe` Right ()
