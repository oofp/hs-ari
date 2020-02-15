module Utils.DTMF where

import  Data.Aeson
import  Protolude

data DTMFDigit = DTMF_1 | DTMF_2 |  DTMF_3 | DTMF_4 | DTMF_5 | DTMF_6 |  DTMF_7 | DTMF_8 | DTMF_9 |  DTMF_0 | DTMF_A | DTMF_B | DTMF_C | DTMF_D | DTMF_STAR | DTMF_POUND deriving (Eq, Show, Bounded, Ord, Enum)
type DTMFBuffer = [DTMFDigit]
emptyDTMDBuffer :: DTMFBuffer
emptyDTMDBuffer = []
dtmfDigitsOnly :: DTMFBuffer
dtmfDigitsOnly = enumFromTo DTMF_1 DTMF_0

dtmfFromText :: Text -> Maybe DTMFDigit
dtmfFromText "1" = Just DTMF_1
dtmfFromText "2" = Just DTMF_2
dtmfFromText "3" = Just DTMF_3
dtmfFromText "4" = Just DTMF_4
dtmfFromText "5" = Just DTMF_5
dtmfFromText "6" = Just DTMF_6
dtmfFromText "7" = Just DTMF_7
dtmfFromText "8" = Just DTMF_8
dtmfFromText "9" = Just DTMF_9
dtmfFromText "0" = Just DTMF_0
dtmfFromText "A" = Just DTMF_A
dtmfFromText "B" = Just DTMF_B
dtmfFromText "C" = Just DTMF_C
dtmfFromText "D" = Just DTMF_D
dtmfFromText "*" = Just DTMF_STAR
dtmfFromText "#" = Just DTMF_POUND
dtmfFromText _ = Nothing

textFromDTMF :: DTMFDigit -> Text
textFromDTMF   DTMF_1       =  "1"
textFromDTMF   DTMF_2       =  "2"
textFromDTMF   DTMF_3       =  "3"
textFromDTMF   DTMF_4       =  "4"
textFromDTMF   DTMF_5       =  "5"
textFromDTMF   DTMF_6       =  "6"
textFromDTMF   DTMF_7       =  "7"
textFromDTMF   DTMF_8       =  "8"
textFromDTMF   DTMF_9       =  "9"
textFromDTMF   DTMF_0       =  "0"
textFromDTMF   DTMF_A       =  "A"
textFromDTMF   DTMF_B       =  "B"
textFromDTMF   DTMF_C       =  "C"
textFromDTMF   DTMF_D       =  "D"
textFromDTMF   DTMF_STAR    =  "*"
textFromDTMF   DTMF_POUND   =  "#"

instance FromJSON DTMFDigit where
  parseJSON (String txt)   =  case dtmfFromText txt of
                                (Just dtmf) -> pure dtmf
                                Nothing ->     mempty
  parseJSON _          =      mempty
