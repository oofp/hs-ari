{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Comm.CommGate
    (
      initCommGate,
      createChannels,
      printHandshake,
      ScenarioFactory,
      ChannelsUpdater
    )
    where


import Protolude
import Control.Concurrent.STM.TChan
import Utils
import Prelude (String)
import qualified Data.UUID.V4 as UUIDGen
import qualified StmContainers.Map as STMMap
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson hiding (defaultOptions)
import Control.Concurrent.Thread.Delay
import Elm.Derive
import Elm.Module

loggerPath::String
loggerPath=getLoggerPath "CommGate"

serverMajorVersion :: Int
serverMajorVersion = 1

data SWVersion = SWVersion
  { majorVersion :: Int
  , midVersion :: Int
  , minVersion :: Int
  } deriving (Eq,Show,Generic)

data SessionHandshake = HaveSessionQ
                      | NewSession SWVersion
                      | SessionExist SWVersion Text
                      | UpgradeRequired
                      | ContinueSession Text
                      | StartSession Text deriving (Eq,Show,Generic)
--instance ToJSON SessionHandshake
--instance FromJSON SessionHandshake
deriveBoth defaultOptions ''SWVersion
deriveBoth defaultOptions ''SessionHandshake

type LBStr =LBS.ByteString

type ChannelsPair = (TChan LBStr, TChan LBStr)  -- ougoing, incoming

type ChannelsUpdater = ChannelsPair  -> IO ()
-- IO() is callback for termination notification;
-- return updater for the folowing channels replacement
type ScenarioFactory = ChannelsPair -> IO () -> IO ChannelsUpdater

type CommGateEntry = ChannelsUpdater

data CommGate = CommGate
  { scenarios :: STMMap.Map Text CommGateEntry
  , scenarioFactory :: ScenarioFactory
  }

initCommGate :: ScenarioFactory -> IO CommGate
initCommGate scenFactory = do
  scs <- STMMap.newIO
  return $ CommGate scs scenFactory

createChannels :: CommGate -> IO ChannelsPair
createChannels commGate = do
  chOut <- newTChanIO
  chIn <- newTChanIO
  let pair =(chOut, chIn)
  _ <- async $ handleComm 0 commGate pair
  return pair

sendHandshake :: TChan LBStr -> SessionHandshake -> IO ()
sendHandshake chOut sh =
  atomically $ writeTChan chOut (encode sh)

getNextHandshake :: TChan LBStr -> IO (Maybe SessionHandshake)
getNextHandshake chIn = do
  bytes <- atomically $ readTChan chIn
  case decode bytes of
    Nothing   -> warningM loggerPath ("Failed to decode: " <> show bytes) >> return Nothing
    (Just hs) -> return hs


handleComm :: Integer -> CommGate -> ChannelsPair -> IO ()
handleComm timeout commGate pair@(chOut, chIn) =
  let send = sendHandshake chOut
      recv = getNextHandshake chIn
      nextTimeout
        | timeout==0 = 1000000
        | otherwise = timeout * 2
  in do
    delay timeout
    send HaveSessionQ
    haveSessionResp <- recv
    case haveSessionResp of
      Nothing -> sendHandshake chOut UpgradeRequired
      Just (NewSession version)  -> ensureVersion version commGate pair createNewSession
      Just (SessionExist version sessionID) -> ensureVersion version commGate pair (continueSession sessionID)
      Just otherHs -> warningM loggerPath ("Unexpected msg: " <> show otherHs) >> handleComm nextTimeout commGate pair

ensureVersion :: SWVersion -> CommGate -> ChannelsPair ->  (CommGate -> ChannelsPair -> IO ()) -> IO ()
ensureVersion clientVersion commGate pair@(chOut, _chIn) sessionReqHandler=
  if serverMajorVersion > (majorVersion clientVersion)
    then
      sendHandshake chOut UpgradeRequired
    else
      sessionReqHandler commGate pair

createNewSession :: CommGate -> ChannelsPair -> IO ()
createNewSession commGate pair@(chOut, _chIn) = do
  uuid <- UUIDGen.nextRandom
  let sessionID=show uuid
  infoM loggerPath ("Session: " <> show sessionID <> " created")
  sendHandshake chOut (StartSession sessionID)  -- from this point we must not read chIn here (as we hand it off to scenario)
  scenarioUpdater <- scenarioFactory commGate pair (removeScenario commGate sessionID) -- potential race when entry is deleted from map before it is added
  atomically $ STMMap.insert scenarioUpdater sessionID (scenarios commGate)


continueSession :: Text -> CommGate -> ChannelsPair -> IO ()
continueSession sessionID commGate pair@(chOut, _chIn) = do
  scenarioUpdaterMaybe <- atomically $ STMMap.lookup sessionID (scenarios commGate)
  case scenarioUpdaterMaybe of
    Just scenarioUpdater -> infoM loggerPath ("Update session: " <> show sessionID) >>
                            scenarioUpdater pair >>
                            sendHandshake chOut (ContinueSession sessionID)

    Nothing ->              infoM loggerPath ("Session: " <> show sessionID <> " not found. Will create new") >>
                            createNewSession commGate pair


removeScenario :: CommGate -> Text -> IO ()
removeScenario commGate sessionID =
  atomically $ STMMap.delete sessionID (scenarios commGate)

printHandshake :: IO ()
printHandshake = putStrLn $ makeElmModule "SessionHandshake"
  [ DefineElm (Proxy :: Proxy SWVersion)
  , DefineElm (Proxy :: Proxy SessionHandshake)
  ]
