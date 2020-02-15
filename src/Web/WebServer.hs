module Web.WebServer
  where

import Protolude


import qualified Network.Wai as WAI
import Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.HTTP.Types as HTTPTypes
import Network.Wai.Handler.WarpTLS
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Util
import Network.URI
import Prelude (String)
import qualified Data.Text.Lazy as TL

scottyHandler :: String -> Scotty.ScottyM ()
scottyHandler defPage = do
   --Scotty.get "/" $ Scotty.file ".\\static\\GroupCalls.html"
   Scotty.get "/" $ Scotty.file defPage
   Scotty.get "/login"  handleLogin
   Scotty.get "/auth"  handleAuth
   Scotty.middleware $ staticPolicy (noDots >-> addBase "static")

waiScottyApp :: String -> IO WAI.Application
waiScottyApp defPage = Scotty.scottyApp (scottyHandler defPage)

webApp :: String -> WS.ServerApp->IO WAI.Application
webApp defPage wsApp = do
  httpHandler <- waiScottyApp defPage
  return $ websocketsOr defaultConnectionOptions wsApp httpHandler

runWebApp :: String -> WS.ServerApp->Int->IO ()
runWebApp  defPage wsApp port = do
   --let tlsCfg=tlsSettings "server.crt" "server.key"
   let tlsCfg=tlsSettingsChain "/etc/letsencrypt/live/somedomain.com/cert.pem" ["/etc/letsencrypt/live/somedomain.com/chain.pem"] "/etc/letsencrypt/live/somedomain.com/privkey.pem"
   let cfg=Warp.setPort port Warp.defaultSettings
   appToRun <- webApp defPage wsApp
   runTLS tlsCfg cfg appToRun

runHttpApp :: String -> WS.ServerApp->Int->IO ()
runHttpApp  defPage wsApp port = do
  appToRun <- webApp defPage wsApp
  Warp.run port appToRun


redirApp :: String -> WAI.Application
redirApp toURL _req respond = respond =<< redirect' HTTPTypes.status302 [] uri
 where
   -- Just uri = parseURI "https://localhost:443/GroupCalls.html"
   -- Just uri = parseURI "https://voip2.me/GroupCalls.html"
   Just uri = parseURI toURL

runRedirect :: Int -> String -> IO ()
runRedirect port toURL = Warp.run port (redirApp toURL)

{-
handleTokenReceived :: String -> Scotty.ActionM ()
handleTokenReceived _defPage = do
  --token <- Scotty.param "access_token"
  --let uriStr :: TL.Text
  --    uriStr = ("https://somedomain.com/wc.html?gottoken"::TL.Text) <> token
  --Scotty.redirect uriStr
  req <- Scotty.request
  bodyBytes <- liftIO $ Network.Wai.requestBody req
  --bodyBytes <- body
  let bodyTxt=show bodyBytes
  Scotty.text (show req <> " Body:" <> bodyTxt)
-}

handleLogin :: Scotty.ActionM ()
handleLogin =
  Scotty.redirect "https://accounts.google.com/o/oauth2/v2/auth?scope=email%20profile&redirect_uri=https%3A%2F%2Fsomedomain.com%2Fauth&response_type=code&client_id=276147037402-o1360v1kbd2etpsg8q7jb9vvnd47qv33.apps.googleusercontent.com"

handleAuth :: Scotty.ActionM ()
handleAuth = do
  code <- Scotty.param "code"
  let urlStr :: TL.Text
      urlStr = ("https://somedomain.com/wc.html?code"::TL.Text) <> code

  Scotty.redirect urlStr

{-
POST /oauth2/v4/token HTTP/1.1
Host: www.googleapis.com
Content-Type: application/x-www-form-urlencoded

code=4/P7q7W91a-oMsCeLvIaQm6bTrgtp7&
client_id=8819981768.apps.googleusercontent.com&
client_secret={client_secret}&
redirect_uri=https://oauth2.example.com/code&
grant_type=authorization_code

A successful response is returned as a JSON array, similar to the following:

{
  "access_token":"1/fFAGRNJru1FTz70BzhT3Zg",
  "expires_in":3920,
  "token_type":"Bearer"
}

-}
