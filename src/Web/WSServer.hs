{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Web.WSServer
    (
      startWSServer,
      getWSApp
    )
    where
--import qualified Data.Text as T
import Protolude
import Control.Concurrent.STM.TChan
import Utils hiding (filter)
import Prelude (String)
import qualified Data.UUID.V4 as UUIDGen

import qualified Network.WebSockets as WS

loggerPath::String
loggerPath=getLoggerPath "WSServer"

type Client = (String, WS.Connection)

--The state kept on the server is simply a list of connected clients. We've added
--an alias and some utility functions, so it will be easier to extend this state
--later on.

type ServerState = [Client]

--Create a new, initial state:
newServerState :: ServerState
newServerState = []

--Get the number of active clients:
--numClients :: ServerState -> Int
--umClients = length

--Check if a user already exists (based on username):
--clientExists :: Client -> ServerState -> Bool
--clientExists client = any ((== fst client) . fst)

--Add a client (this does not check if the client already exists, you should do
--this yourself using `clientExists`):
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

--Remove a client:
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

--Send a message to all clients, and log it on stdout:
--broadcast :: Text -> ServerState -> IO ()
--broadcast message clients = do
--     T.putStrLn message
--     forM_ clients $ \(_, conn) -> WS.sendTextData conn message

--The main function first creates a new state for the server, then spawns the
--actual server. For this purpose, we use the simple server provided by
--`WS.runServer`.
startWSServer :: (WS.WebSocketsData t, Show t)=>IO (TChan t,TChan t) -> t -> Int -> IO ()
startWSServer flowFactory quitMsg port= do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" port $ tryApplication flowFactory quitMsg state

getWSApp :: (WS.WebSocketsData t, Show t) => IO (TChan t,TChan t) -> t -> IO WS.ServerApp
getWSApp flowFactory quitMsg =
  newMVar newServerState >>= (return . tryApplication flowFactory quitMsg)

    -- Note that `WS.ServerApp` is nothing but a type synonym for

--Our main application has the type:
tryApplication :: (WS.WebSocketsData t, Show t)=>IO (TChan t,TChan t) ->t -> MVar ServerState -> WS.ServerApp
application :: (WS.WebSocketsData t, Show t)=>IO (TChan t,TChan t) ->t -> MVar ServerState -> WS.ServerApp

-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
--application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.
tryApplication flowFactory quitMsg state pending = do
  res <- (try $ application flowFactory quitMsg state pending)::IO (Either SomeException ())
  case res of
    (Left ex) ->  errorM loggerPath ("Exception:" <> show ex)
    (Right _) ->  infoM loggerPath "WS Handling completed"

application flowFactory quitMsg  state  pending= do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

--When a client is succesfully connected, we read the first message. This should
--be in the format of "Hi! I am Jasper", where Jasper is the requested username.

    uuid <- UUIDGen.nextRandom
    let clientID=show uuid
        client=(clientID,conn)
    debugM loggerPath ("Accepting new client"<> clientID)


    flowFactory >>= (\(tchanIn,tchanOut) -> forkChannelToConnTask tchanIn conn>>= (\asyncTask ->
        flip finally (disconnect tchanOut asyncTask client) $ do
         modifyMVar_ state $ \s -> do
             let s' = addClient client s
             --WS.sendTextData conn
             --       (T.pack ("Client ID allocated: "<> clientID))
             return s'
         debugM loggerPath "Going to talk"
         talk conn state client tchanOut
        ))
    where
      --quitMsg    = (":quit"::Text)
      disconnect tchanOut asyncTask client= do
          -- Remove client and return new state
          atomically $ writeTChan tchanOut quitMsg
          cancel asyncTask
          _s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
          debugM loggerPath ("Client disconnected"<>fst client)

      forkChannelToConnTask tchanIn connWs = async $ forever $
        atomically (readTChan tchanIn) >>= WS.sendTextData connWs

--The talk function continues to read messages from a single client until he
--disconnects. All messages are broadcasted to the other clients.

talk :: (WS.WebSocketsData t, Show t) => WS.Connection -> MVar ServerState -> Client -> TChan t->IO ()
talk conn _state (user, _) tchanOut = do
  debugM loggerPath ("start talking to "<>user)
  forever $ do
    rcvMsg <- WS.receiveData conn
    debugM loggerPath ("message received"<>show rcvMsg)
    atomically $ writeTChan tchanOut rcvMsg
