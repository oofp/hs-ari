{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ARICore.PJSIPConfigCmd
  ( AccountConfig (..)
  , PJSIPAccountID
  , pjsipCreateAccountCmd
  , pjsipDeleteAccountCmd
  ) where

import Protolude
import Prelude (String)
import Data.Aeson hiding (defaultOptions)

-- import Utils

import ARICore.HTTPCommand
import ARICore.ARIFlow

import Data.Text
import qualified Data.List as DL
import qualified Data.UUID.V4 as UUIDGen
import Elm.Derive

data AccountConfig= AccountConfig
    { sipUserName::Text
    , sipPassword::Text
    , sipDomain::Text
    , sipDirectMedia::Bool
    , sipEnableRPID::Bool
    , sipExtraConfig :: Maybe [(Text,Text)]
    } deriving (Show , Generic)

--instance ToJSON AccountConfig
--instance FromJSON AccountConfig
deriveBoth defaultOptions ''AccountConfig

type PJSIPAccountID=Text

pjsipUDPTransport::Text
pjsipUDPTransport = "transport-udp"

data AttribValue=AttribValue
      {_attrribute::Text
      ,_value::Text
      } deriving (Show)
instance ToJSON AttribValue where
    toJSON (AttribValue attr val) =
            object ["attribute" .= attr, "value" .= val]
    toEncoding (AttribValue attr val) =
        pairs ("attribute" .= attr <> "value" .= val)

type Attributes=[AttribValue]

data Fields=Fields
            {fields::[AttribValue]
            } deriving (Show, Generic)
instance ToJSON Fields

type AccountInfo = (AccountConfig, PJSIPAccountID)

authAttrs::AccountInfo->Attributes
authAttrs (AccountConfig userName password _ _ _ _, _) =
    [ AttribValue "auth_type" "userpass"
    , AttribValue "username" userName
    , AttribValue "password" password
    ]
aorAttrs::AccountInfo->Attributes
aorAttrs (AccountConfig userName _ domain _ _ _, _) =
    [ AttribValue "contact" ("sip:" <> userName <> "@" <> domain) ]

noYes :: Bool -> Text
noYes False = "no"
noYes True =  "yes"

epAttrs::AccountInfo->Attributes
epAttrs (accountConfig, accountID) =
    [ AttribValue "transport" pjsipUDPTransport
    , AttribValue "from_user" (sipUserName accountConfig)
    , AttribValue "allow" "!all,g722,ulaw,alaw"
    , AttribValue "ice_support" "no"
    , AttribValue "force_rport" "yes"
    , AttribValue "rewrite_contact" "yes"
    , AttribValue "rtp_symmetric" "yes"
    , AttribValue "timers_sess_expires" "180"
    , AttribValue "send_rpid" (noYes $ sipEnableRPID accountConfig)
    , AttribValue "context" "default"
    , AttribValue "direct_media" (noYes $ sipDirectMedia accountConfig)
    , AttribValue "auth" accountID
    , AttribValue "outbound_auth" accountID
    , AttribValue "aors" accountID
    , AttribValue "from_domain" (sipDomain accountConfig)
    ]

type RequestParams=(Text, AccountInfo -> Attributes)
pushRequests::[RequestParams]
pushRequests = [("auth", authAttrs)
               ,("aor",aorAttrs)
               ,("endpoint",epAttrs)
               ]

jsonValFromAttrs::Attributes->Value
jsonValFromAttrs = toJSON . Fields

--loggerPath::String
--loggerPath=getLoggerPath "PJSIPConfigCmd"

pjsipPushReqCmd :: PJSIPAccountID -> AccountConfig -> RequestParams -> FlowContext s HTTPOK
pjsipPushReqCmd accountID accountConfig (reqType, attrs) =
  let reqStr::String
      reqStr=unpack $ mconcat ["asterisk/config/dynamic/res_pjsip/"::Text, reqType, "/",accountID]
      reqBody = jsonValFromAttrs (attrs (accountConfig, accountID))
  in putJsonOp reqStr reqBody

pjsipDelReqCmd::PJSIPAccountID -> Text -> FlowContext s HTTPOK
pjsipDelReqCmd accountID reqType =
  let reqStr::String
      reqStr=unpack$ mconcat ["asterisk/config/dynamic/res_pjsip/", reqType, "/", accountID]
  in delOp reqStr

pjsipCreateAccountCmd:: AccountConfig -> FlowContext s PJSIPAccountID
pjsipCreateAccountCmd accountConfig = do
  accountID <- show <$> liftIO UUIDGen.nextRandom
  forM_ pushRequests  (pjsipPushReqCmd accountID accountConfig)
  return accountID

pjsipDeleteAccountCmd::PJSIPAccountID -> FlowContext s ()
pjsipDeleteAccountCmd accountID =
  forM_ (fst <$> DL.reverse pushRequests)  (pjsipDelReqCmd accountID)
