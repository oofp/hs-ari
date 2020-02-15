{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FlowTools.IVRComps
  ( evalIVRComp
  , evalIVRComp'
  , runIVRComp
  , execIVRComp
  , spawnIVRComp
  , waitDTMF
  , playList
  , playListDTMF
  , ivrMenu
  , collect
  , clearDTMFs
  , getDTMFs
  , getAndClearDTMFs
  , transDTMFLen
  , genericTrans
  , IVRCallbackCont
  , IVRMenuParams (..)
  , CollectParams (..)
  , DTMFTransResult (..)
  ) where

--import Prelude (String)import Control.Monad

import Protolude
import Control.Lens
import Prelude (String)

import Utils
import ARICore
import ARIUtils

import FlowTools.FlowEvDistr
import FlowTools.FlowCallback
import FlowTools.FlowCallbackComps
import FlowTools.DTMFCollector
import FlowTools.TimerTool
import FlowTools.Playlist
import qualified Data.Text as T

loggerPath::String
loggerPath=getLoggerPath "IVRComps"

data IVRState s = IVRState {_ariEventDistr::ARIEventDistributor (IVRState s), _dtmfCollector :: DTMFCollector (IVRState s),  _connectedChannel :: ConnectedChannel, _ivrInternState::s}
type IVRCallbackCont s a = FlowCallback (IVRState s) a
type IVRFlowContext s a = FlowContext (IVRState s) a
makeLenses ''IVRState

instance HasARIEventDistributor (IVRState s)  where
  getEventDistributor = ariEventDistr

newIVRState :: (EmptyData s) => ConnectedChannel -> IVRState s
newIVRState connectedChan = IVRState newEventDistributor newDTMFCollector connectedChan emptyData

channelHandle :: IVRFlowContext s ChannelHandle
channelHandle = do
  (ChannelInState channelHnd) <- use connectedChannel
  return channelHnd

--useChannelHandle :: IVRCallbackCont s ChannelHandle
--useChannelHandle = lift channelHandle

useConnectedChannel :: IVRCallbackCont s ConnectedChannel
useConnectedChannel = lift (use connectedChannel)

preIVRFlow :: IVRFlowContext s ()
preIVRFlow  = do
  res <- channelHandle >>= enableHanldeMonitoring
  case res of
    Nothing ->  throwError $ GenericError "Failed to enableHanldeMonitoring"
    (Just _) -> connectedChannel .$ startDTMFCollector dtmfCollector

invokeIVRComp :: (EmptyData s1) => IVRCallbackCont s1 a -> ConnectedChannel -> InvokeCallCompFunc s (IVRState s1) a a_s1 -> FlowCallback s a_s1
invokeIVRComp ivrCall connectedChan invokeCallFunc=
  let initState = newIVRState connectedChan
  in invokeCallFunc ivrCall preIVRFlow initState

evalIVRComp :: (EmptyData s1) => IVRCallbackCont s1 a -> ConnectedChannel -> FlowCallback s a
evalIVRComp ivrCall connectedChan = invokeIVRComp ivrCall connectedChan evalCallComp

evalIVRComp' :: IVRCallbackCont () a -> ConnectedChannel -> FlowCallback s a
evalIVRComp' = evalIVRComp

runIVRComp :: (EmptyData s1) => IVRCallbackCont s1 a -> ConnectedChannel -> FlowCallback s (a,s1)
runIVRComp ivrCall connectedChan = (\(a,ivrState) -> (a, ivrState ^. ivrInternState)) <$> invokeIVRComp ivrCall connectedChan runCallComp

execIVRComp :: (EmptyData s1) => IVRCallbackCont s1 a -> ConnectedChannel -> FlowCallback s s1
execIVRComp ivrCall connectedChan = (^. ivrInternState) <$> invokeIVRComp ivrCall connectedChan execCallComp

spawnIVRComp :: (EmptyData s1) => IVRCallbackCont s1 a -> ConnectedChannel -> FlowCallback s ()
spawnIVRComp ivrCall conChannel =
  let wrappedIVRCall = ((useConnectedChannel >>= waitForChannelTerminatedEvent) >||< (ivrCall >> lift (use connectedChannel >>= dropChannel)))
  in void $ execIVRComp wrappedIVRCall conChannel

wrap :: (ConnectedChannel -> IVRCallbackCont s a) -> IVRCallbackCont s a
wrap cbComp = lift (use connectedChannel) >>= cbComp

playList :: Playlist -> IVRCallbackCont s ()
playList  = wrap . playListComp

waitDTMF :: IVRCallbackCont s DTMFDigit
waitDTMF = createCallbackCont (\cbFunc ->
  do
    eventID <- addAnyDTMFMonitor (\_ d->(cbFunc d >> return False))  dtmfCollector
    return $ removeDTMFMonitor eventID dtmfCollector)

waitDTMFTimer :: Integer -> IVRCallbackCont s (Maybe DTMFDigit)
waitDTMFTimer timeout = eitherToMaybe <$> raceCall (timeCallback timeout) waitDTMF

playListDTMF :: Integer -> Playlist -> IVRCallbackCont s (Maybe DTMFDigit)
playListDTMF timeout files = eitherToMaybe <$> raceCall (playList files >> timeCallback timeout) waitDTMF

playListDTMFNoDelay :: Playlist -> IVRCallbackCont s (Maybe DTMFDigit)
playListDTMFNoDelay files = eitherToMaybe <$> raceCall (playList files) waitDTMF

clearDTMFs :: IVRCallbackCont s ()
clearDTMFs = lift $ clearDTMFBuffer dtmfCollector

getDTMFs :: IVRCallbackCont s DTMFBuffer
getDTMFs = lift $ getDTMFBuffer dtmfCollector

getAndClearDTMFs :: IVRCallbackCont s DTMFBuffer
getAndClearDTMFs = lift $ getAndClearDTMFBuffer dtmfCollector

data IVRMenuParams = IVRMenuParams
  { _promptPlaybacks :: Playlist
  , _wrongKeyPlaybacks :: Playlist
  , _timeoutPlaybacks :: Playlist
  , _validKeys :: DTMFBuffer
  , _repeatKey :: Maybe DTMFDigit
  , _maxAttempts :: Int
  , _timeout :: Integer
  }
makeLenses ''IVRMenuParams

ivrMenu :: IVRMenuParams ->  IVRCallbackCont s (Maybe DTMFDigit)
ivrMenu params =
    go (params^.maxAttempts) []
  where
    go attemptsLeft prePlay
      | attemptsLeft > 0 =
        do
          liftIO $ debugM loggerPath ("ivrMenu: attempts left:" <> show attemptsLeft <> " ; pre-play:" <> show prePlay)
          playRes <- playListDTMF (params ^.timeout) (prePlay <> (params ^.promptPlaybacks))
          case playRes of
            Just dgt | dgt `elem` (params ^.validKeys)     -> return $ Just dgt
                     |  Just dgt == (params ^. repeatKey)  -> go (params^.maxAttempts) []
                     |  otherwise                          -> go (attemptsLeft-1) (params ^.wrongKeyPlaybacks)
            Nothing -> go (attemptsLeft-1) (params ^.timeoutPlaybacks)
      | otherwise = return Nothing

data CollectParams = CollectParams
  { _collectPromptPlaybacks :: Playlist
  , _collectWrongInputPlaybacks :: Playlist --also played for inter-digit timeout
  , _collectTimeoutPlaybacks :: Playlist    --applies for 1st digit timeout
  , _collectMaxAttempts :: Int
  , _collectInterdigitTimeout :: Integer
  , _collectInitTimeout :: Integer
  }
makeLenses ''CollectParams

data DTMFTransResult a = CollectContinue | CollectAttemptFailed | CollectExit | CollectRestart | CollectRetry | CollectDone a deriving (Show, Eq)

type DTMFInterpreter a = DTMFBuffer -> DTMFTransResult a

textFromDTMFs :: DTMFBuffer -> Text
textFromDTMFs dtmfBuf = T.concat $  fmap textFromDTMF dtmfBuf

transDTMFLen :: Int -> DTMFInterpreter Text
transDTMFLen digitsNumber dtmfBuf =
   let curLen = length dtmfBuf
   in
      if curLen < digitsNumber
        then CollectContinue
        else if curLen == digitsNumber
          then CollectDone $ textFromDTMFs dtmfBuf
          else CollectAttemptFailed -- not supposed to happen; unless it was at the buffer before

genericTrans :: Int -> Int -> Maybe DTMFBuffer -> Maybe (DTMFDigit, Bool) -> DTMFInterpreter Text
genericTrans minLen maxLen allowedDigitsMaybe termDigitMaybe dtmfBuf =
    fromMaybe  CollectContinue (getResult $ foldl' checkDigit (0,Nothing) dtmfBuf)
  where
    isTermRequired = fromMaybe False (snd <$> termDigitMaybe)
    isAllowed dtmfDigit = fromMaybe True (elem dtmfDigit <$> allowedDigitsMaybe)
    isTerm = case termDigitMaybe of
      Nothing -> const False
      Just (termDigit , _) -> (== termDigit)
    incDigitCounter (curLen,res) = (curLen+1, res)
    getResult  = snd
    failCollect (curLen,_)  = (curLen+1, Just CollectAttemptFailed)
    completeCollect (curLen,_) inc =
      let finalLen = curLen+inc
      in (finalLen, Just $ CollectDone $ textFromDTMFs $ take finalLen dtmfBuf)
    checkDigit :: (Int,Maybe (DTMFTransResult Text)) -> DTMFDigit -> (Int,Maybe (DTMFTransResult Text))
    checkDigit val@(curLen, resMaybe) dtmfDigit
      | isJust resMaybe                                     = val
      | curLen < minLen && isTerm  dtmfDigit                = failCollect val
      | curLen + 1 < minLen && not (isAllowed dtmfDigit)    = failCollect val
      | curLen == maxLen && not (isTerm  dtmfDigit)         = failCollect val  -- can happen only if  term digit must be used
      | not (isAllowed dtmfDigit) && not (isTerm dtmfDigit) = failCollect val
      | curLen >= minLen && isTerm dtmfDigit                = completeCollect val 0 -- term detected
      | curLen+1 >= minLen && curLen == maxLen-1
          && not isTermRequired                             = completeCollect val 1 -- reached max
      | otherwise                                           = incDigitCounter val

collect :: CollectParams -> DTMFInterpreter a ->  IVRCallbackCont s (Maybe a)
collect params transFunc = do
     currentDigits <- getDTMFs
     case currentDigits of
       [] -> startNewInput (params ^. collectMaxAttempts) []
       _ -> analyzeInput (params ^. collectMaxAttempts)
  where
    startNewInput 0 _ = return Nothing -- no more attempts left
    startNewInput attemptsLeft prePlay= do
      clearDTMFs
      unless (null prePlay) (playList prePlay) -- no interruptible
      playRes <- playListDTMFNoDelay (params ^.collectPromptPlaybacks)
      case playRes of
        Nothing -> waitForFirstDigit attemptsLeft
        Just _ ->  analyzeInput attemptsLeft
    waitForFirstDigit  attemptsLeft = do
      waitInitRes <- waitDTMFTimer (params ^. collectInitTimeout)
      case waitInitRes of
        Nothing -> startNewInput (attemptsLeft-1) (params ^. collectTimeoutPlaybacks)
        (Just _) -> analyzeInput attemptsLeft
    waitForNextDigit  attemptsLeft = do
      waitNextRes <- waitDTMFTimer (params ^. collectInterdigitTimeout)
      case waitNextRes of
        Nothing -> startNewInput (attemptsLeft-1) (params ^. collectWrongInputPlaybacks)
        (Just _) -> analyzeInput attemptsLeft
    analyzeInput attemptsLeft = do
      currentDigits <- getDTMFs
      case transFunc currentDigits of
        CollectContinue -> waitForNextDigit attemptsLeft
        CollectAttemptFailed -> startNewInput (attemptsLeft-1) []
        CollectExit -> return Nothing
        CollectRestart -> startNewInput (params ^. collectMaxAttempts) []
        CollectRetry -> startNewInput attemptsLeft []
        (CollectDone a) -> return $ Just a

data ConfirmParams = ConfirmParams
  { _confirmPromptPlaybacks :: Playlist
  , _confirmWrongKeyPlaybacks :: Playlist
  , _confirmTimeoutPlaybacks :: Playlist
  , _confirmKey :: DTMFDigit
  , _rejectKey :: DTMFDigit
  , _confirmRepeatKey :: Maybe DTMFDigit
  , _confirmMaxAttempts :: Int
  , _confirmTimeout :: Integer
  }
makeLenses ''ConfirmParams

transToMenuParams :: ConfirmParams -> IVRMenuParams
transToMenuParams confirmParams =
  IVRMenuParams
    { _promptPlaybacks = confirmParams ^. confirmPromptPlaybacks
    , _wrongKeyPlaybacks = confirmParams ^. confirmWrongKeyPlaybacks
    , _timeoutPlaybacks =  confirmParams ^. confirmTimeoutPlaybacks
    , _validKeys = [confirmParams ^. confirmKey, confirmParams ^. rejectKey]
    , _repeatKey = confirmParams ^. confirmRepeatKey
    , _maxAttempts = confirmParams ^. confirmMaxAttempts
    , _timeout = confirmParams ^. confirmTimeout
    }


collectConfirm :: CollectParams -> (DTMFBuffer ->DTMFTransResult a) -> (a->ConfirmParams) -> IVRCallbackCont s (Maybe a)
collectConfirm collectParams transFunc confirmParamsFunc = do
  aMaybe <- collect collectParams transFunc
  case aMaybe of
    Nothing -> return Nothing
    (Just a) -> do
      let confirmParams =confirmParamsFunc a
      digitMaybe <- ivrMenu (transToMenuParams confirmParams)
      case digitMaybe of
        Just d  | d == (confirmParams ^. confirmKey) -> return  $ Just a
                | d == (confirmParams ^. rejectKey)  -> collectConfirm collectParams transFunc confirmParamsFunc
                | otherwise -> return Nothing -- cannot happen
        Nothing -> return Nothing

{-
updateCallbackResult :: s -> IVRFlow s ()
updateCallbackResult s = ivrRes .= s

runIVRComp :: IVRCallbackCont s s -> IVRFlow s (CallContext (IVRFlowM s))
runIVRComp ivrComp = runCall ivrComp updateCallbackResult

createIVRFlow :: (EmptyData s) => ConnectedChannel -> IVRCallbackCont s s -> IVRFlow s (CallContext (IVRFlowM s))
createIVRFlow connectedChannel ivrComp = preIVRFlow connectedChannel >> runIVRComp ivrComp

--runIVRCall :: (EmptyData s) => FlowData (IVRState s) -> ConnectedChannel -> IVRCallbackCont s -> IO (Either FlowError (),  IVRState s)
--runIVRCall fd connectedChannel@(ChannelInState channelHnd) ivrComp = runFlowLoop ((preIVRFlow connectedChannel) >> (runCall ivrComp put)) fd (newIVRState channelHnd)
-}
