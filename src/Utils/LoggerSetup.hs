module Utils.LoggerSetup
(
  setupLog,
  setupLog2,
  getLoggerPath,
  rootLog,
  debugM,
  warningM,
  infoM,
  errorM
)
where

import System.Log.Logger
--import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Protolude
import Prelude (String)

rootLog::String
rootLog="hAri"

getLoggerPath::String->String
getLoggerPath  logger = mconcat [rootLog , ".", logger]

setupLog::IO()
setupLog=do
  updateGlobalLogger rootLog (setLevel DEBUG)
  h <- fileHandler "hAri.log" DEBUG >>= \lh -> return $
              setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  -- s <- openlog "SyslogStuff" [PID] USER DEBUG
  -- updateGlobalLogger rootLoggerName (addHandler s)

  updateGlobalLogger rootLog (addHandler h)

  debugM rootLog "Logger started"

setupLog2::IO()
setupLog2 = do
  let logPath = "sipTest.log"
  myStreamHandler <- streamHandler stderr INFO
  myFileHandler <- fileHandler logPath DEBUG
  let myFileHandler' = withFormatter myFileHandler
  let myStreamHandler' = withFormatter myStreamHandler
  let logger = rootLoggerName
  updateGlobalLogger logger (setLevel DEBUG)
  updateGlobalLogger logger (setHandlers [myFileHandler', myStreamHandler'])


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler myFormatter
    -- http://hackage.haskell.org/packages/archive/hslogger/1.1.4/doc/html/System-Log-Formatter.html
    where myFormatter = simpleLogFormatter "[$time $loggername $prio] $msg"
