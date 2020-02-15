module DTMFTransTest (dtmfTransTest) where

--import qualified Data.ByteString as BStr
import           Protolude
import           Data.Aeson
import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception (evaluate)
import           FlowTools.IVRComps
import           Utils

dtmfTransTest :: IO ()
dtmfTransTest = hspec $ do
  describe "transDTMFLen" $ do
    it "exact match test" $ do
      let transRes = transDTMFLen 4 [DTMF_1,DTMF_2,DTMF_3, DTMF_4]
      transRes `shouldBe` CollectDone "1234"
    it "empty buffer test" $ do
      let transRes = transDTMFLen 4 []
      transRes `shouldBe` CollectContinue
    it "short buffer test" $ do
      let transRes = transDTMFLen 4 [DTMF_1,DTMF_2,DTMF_3]
      transRes `shouldBe` CollectContinue
    it "longer buffer test" $ do
      let transRes = transDTMFLen 4 [DTMF_1,DTMF_2,DTMF_3, DTMF_4, DTMF_5]
      transRes `shouldBe` CollectAttemptFailed
  describe "genericTrans" $ do
    it "exact match test" $ do
      let transRes = genericTrans 4 4 Nothing Nothing [DTMF_1,DTMF_2,DTMF_3, DTMF_4]
      transRes `shouldBe` CollectDone "1234"
    it "empty buffer test" $ do
      let transRes = genericTrans 4 4 Nothing Nothing []
      transRes `shouldBe` CollectContinue
    it "short buffer test" $ do
      let transRes = genericTrans 4 4 Nothing Nothing  [DTMF_1,DTMF_2,DTMF_3]
      transRes `shouldBe` CollectContinue
    it "longer buffer test" $ do
      let transRes = genericTrans 4 4 Nothing Nothing  [DTMF_1,DTMF_2,DTMF_3, DTMF_4, DTMF_5]
      transRes `shouldBe` CollectDone "1234"
    it "illegal digit test at the end" $ do
      let transRes = genericTrans 4 4 (Just dtmfDigitsOnly) Nothing [DTMF_1,DTMF_2,DTMF_3, DTMF_POUND]
      transRes `shouldBe` CollectAttemptFailed
    it "illegal digit test at the start" $ do
      let transRes = genericTrans 4 4 (Just dtmfDigitsOnly) Nothing [DTMF_POUND, DTMF_1,DTMF_2,DTMF_3]
      transRes `shouldBe` CollectAttemptFailed
    it "term digit in the beginning" $ do
      let transRes = genericTrans 4 4 (Just dtmfDigitsOnly) (Just (DTMF_POUND,True)) [DTMF_POUND, DTMF_1,DTMF_2,DTMF_3]
      transRes `shouldBe` CollectAttemptFailed
    it "term digit too early 4-4" $ do
      let transRes = genericTrans 4 4 (Just dtmfDigitsOnly) (Just (DTMF_POUND,True)) [DTMF_1,DTMF_2,DTMF_3, DTMF_POUND]
      transRes `shouldBe` CollectAttemptFailed
    it "term digit too early 4-5" $ do
      let transRes = genericTrans 4 5 (Just dtmfDigitsOnly) (Just (DTMF_POUND,True)) [DTMF_1,DTMF_2,DTMF_3, DTMF_POUND]
      transRes `shouldBe` CollectAttemptFailed
    it "term digit too late" $ do
      let transRes = genericTrans 4 5 (Just dtmfDigitsOnly) (Just (DTMF_POUND,True)) [DTMF_1,DTMF_2,DTMF_3, DTMF_1,DTMF_2,DTMF_3, DTMF_POUND]
      transRes `shouldBe` CollectAttemptFailed
    it "match by length failed" $ do
      let transRes = genericTrans 4 5 (Just dtmfDigitsOnly) (Just (DTMF_POUND,True))  [DTMF_1,DTMF_2,DTMF_3, DTMF_4, DTMF_5, DTMF_5]
      transRes `shouldBe` CollectAttemptFailed
    it "match by length succeeded" $ do
      let transRes = genericTrans 4 5 (Just dtmfDigitsOnly) (Just (DTMF_POUND,False))  [DTMF_1,DTMF_2,DTMF_3, DTMF_4, DTMF_5]
      transRes `shouldBe` CollectDone "12345"
    it "illegal star" $ do
      let transRes = genericTrans 4 5 (Just dtmfDigitsOnly) (Just (DTMF_POUND,False))  [DTMF_1,DTMF_2,DTMF_STAR, DTMF_4, DTMF_5]
      transRes `shouldBe` CollectAttemptFailed
    it "match by term pound succeeded" $ do
      let transRes = genericTrans 4 8 (Just dtmfDigitsOnly) (Just (DTMF_POUND,False))  [DTMF_1,DTMF_2,DTMF_3, DTMF_4, DTMF_5, DTMF_POUND]
      transRes `shouldBe` CollectDone "12345"
