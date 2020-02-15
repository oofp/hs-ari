{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ARICore.HTTPCommand
(
  mkHTTPEnv,
  getOp,
  postOp,
  postJsonOp,
  putJsonOp,
  delOp,
  getApp,
  suppressError,
  FromHTTPError (..),
  HTTPEnvProvider (..),
  HTTPResp,
  HTTPReq,
  HTTPEnv (..),
  HTTPOK (..),
  HTTPError
)
where

import Protolude
import Prelude (String)
import ARICore.HTTPOpts (getHTTPOpts)
import Network.Wreq (Response,responseStatus,statusCode,responseBody,getWith,deleteWith,postWith,putWith,Options) 
import Control.Lens
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBStr
import qualified Control.Monad.Trans.Reader as R
import qualified Data.ByteString.Char8 as C8
import Utils.LoggerSetup
import qualified  Network.HTTP.Client as HTTPClient

-- TODO: Change String to text as URL parameter (or avoid using string in other way)
--opts::Options
--opts=getHTTPOpts "boris" "boris"
loggerPath::String
loggerPath=getLoggerPath "HTTPCommand"

httpReqWithL :: String->(Options -> String -> IO (Response LBStr.ByteString))->Options->String-> IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
httpReqWithL  label httpReq opts reqStr = do
  debugM loggerPath (mconcat ["Request ", label, reqStr])
  resp <- try $ httpReq opts reqStr
  --let status=resp ^. responseStatus
  --    code=status ^. statusCode
  --debugM loggerPath ("Response code:"<>show code)
  --debugM loggerPath ("Response status:"<>show status)
  debugM loggerPath (mconcat["Response ", label, show resp])
  return resp

httpJsonReqWithL :: String->(Options -> String -> Value -> IO (Response LBStr.ByteString))->Options->String-> Value -> IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
httpJsonReqWithL  label httpJsonReq opts reqStr jsonBody= do
  debugM loggerPath (mconcat ["Request ", label, reqStr," body:", show jsonBody])
  resp <- try $ httpJsonReq opts reqStr jsonBody
  --let status=resp ^. responseStatus
  --    code=status ^. statusCode
  --debugM loggerPath ("Response code:"<>show code)
  --debugM loggerPath ("Response status:"<>show status)
  debugM loggerPath (mconcat["Response ", label, show resp])
  return resp

type HTTPError = HTTPClient.HttpException
data HTTPOK = HTTPOK {jsonVal::Maybe Value} deriving (Eq,Show)
--data HTTPError = HTTPError {respCode::Int, statusMsg::BStr.ByteString} deriving (Eq,Show)
type HTTPResp= Either HTTPError HTTPOK

type HTTPReq = String->IO HTTPResp
type HTTPJsonReq = String->Value->IO HTTPResp
data HTTPEnv = HTTPEnv {postCmd::HTTPReq, postJsonCmd::HTTPJsonReq, putJsonCmd::HTTPJsonReq, delCmd::HTTPReq, getCmd::HTTPReq, app::String}

handleResp::Either HTTPClient.HttpException (Response LBStr.ByteString) -> HTTPResp
handleResp  respEither =
    case respEither of
      (Right resp) -> fromResponse resp
      (Left ex) -> Left ex
  where fromResponse resp =
          let status=resp ^. responseStatus
              _code=status ^. statusCode
              respBodyBytes=resp ^. responseBody
              respBodyVal= decode respBodyBytes::(Maybe Value)
          in Right $ HTTPOK respBodyVal


--mkHTTPEnv::String->String->BStr.ByteString->BStr.ByteString->HTTPEnv
getWithL::Options -> String -> IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
getWithL=httpReqWithL "GET:" getWith
delWithL::Options -> String -> IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
delWithL=httpReqWithL "DEL:" deleteWith
postWithL::Options -> String -> IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
postWithL=httpReqWithL "POST:" (\opts reqStr->postWith opts reqStr LBStr.empty)
postJsonWithL::Options -> String -> Value ->IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
postJsonWithL=httpJsonReqWithL "POST:" postWith
putJsonWithL::Options -> String -> Value ->IO (Either HTTPClient.HttpException (Response LBStr.ByteString))
putJsonWithL=httpJsonReqWithL "PUT:" putWith

mkHTTPEnv::String->String->String->String->HTTPEnv
mkHTTPEnv appName baseURL user pwd=
    let httpOpts=getHTTPOpts (C8.pack user) (C8.pack pwd)
        fullURL pars=baseURL++pars
        postReq reqPars= handleResp <$> postWithL httpOpts (fullURL reqPars)
        postJsonReq reqPars bodyVal = handleResp <$> postJsonWithL httpOpts (fullURL reqPars) bodyVal
        putJsonReq reqPars bodyVal = handleResp <$> putJsonWithL httpOpts (fullURL reqPars) bodyVal
        delReq reqPars= handleResp <$> delWithL httpOpts (fullURL reqPars)
        getReq reqPars= handleResp <$> getWithL httpOpts (fullURL reqPars)
    in HTTPEnv postReq postJsonReq putJsonReq delReq getReq appName


class HTTPEnvProvider a where
  getHTTPEnv::a->HTTPEnv

type HTTPTrans e s r a = ExceptT e (StateT s(ReaderT r IO)) a
type HTTPOp e s r = HTTPTrans e s r HTTPOK

getApp ::(HTTPEnvProvider r)=>HTTPTrans e s r String
getApp = do
    envProv <- (lift.lift) R.ask
    let env=getHTTPEnv envProv
    return $ app env

class FromHTTPError e where
  fromHTTPError::HTTPError->e

transFromHTTPResp::(FromHTTPError e)=>HTTPResp->Either e HTTPOK
transFromHTTPResp (Left httpErr) = Left (fromHTTPError httpErr)
transFromHTTPResp (Right httpOK) = Right httpOK

httpOp :: (HTTPEnvProvider r, FromHTTPError e)=> (HTTPEnv->HTTPReq)->String->HTTPOp e s r
httpOp operation cmdStr =
     ExceptT $ lift $ ReaderT (\envProv->fmap transFromHTTPResp (operation (getHTTPEnv envProv) cmdStr))

httpJsonOp :: (HTTPEnvProvider r, FromHTTPError e)=> (HTTPEnv->HTTPJsonReq)->String->Value->HTTPOp e s r
httpJsonOp operation cmdStr jsonBody =
    ExceptT $ lift $ ReaderT (\envProv->fmap transFromHTTPResp (operation (getHTTPEnv envProv) cmdStr jsonBody))
getOp::(HTTPEnvProvider r, FromHTTPError e)=>String->HTTPOp e s r
getOp = httpOp getCmd
postOp::(HTTPEnvProvider r , FromHTTPError e)=>String->HTTPOp e s r
postOp = httpOp postCmd
postJsonOp::(HTTPEnvProvider r , FromHTTPError e)=>String->Value->HTTPOp e s r
postJsonOp = httpJsonOp postJsonCmd
putJsonOp::(HTTPEnvProvider r , FromHTTPError e)=>String->Value->HTTPOp e s r
putJsonOp = httpJsonOp putJsonCmd
delOp::(HTTPEnvProvider r, FromHTTPError e)=>String->HTTPOp e s r
delOp = httpOp delCmd

suppressError :: HTTPOp e s HTTPOK -> HTTPOp e s HTTPOK
suppressError httpOpr = 
  catchError httpOpr (\_e -> return $ HTTPOK Nothing)

