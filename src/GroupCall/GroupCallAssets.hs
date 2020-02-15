{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE LiberalTypeSynonyms #-}
--{-# LANGUAGE ImpredicativeTypes  #-}

module GroupCall.GroupCallAssets where

import Protolude hiding (handle)
import Control.Lens
import Data.Map.Strict
import qualified Data.Map.Strict as M

import ARICore hiding (bridgeID, recording)

import GroupCall.GroupCallData
data SnoopingState = SnoopingState
  { _spyDir :: SpyDir
  , _whisperDir :: WhisperDir
  , _snoopChannel :: ChannelHandle
  -- , _gcSnoopBridgeID :: Maybe BridgeID --already exists at GroupCall
  }

makeLenses ''SnoopingState

data GroupCallEntry = GroupCallEntry
  { _gcCall :: GroupCall
  , _handle :: ChannelHandle
  , _snoopingState :: Maybe SnoopingState
  }
makeLenses ''GroupCallEntry

data RecorderEntry = RecorderEntry
  { _recording :: CallRecording
  , _recordingHandle :: RecordingHandle
  }
makeLenses ''RecorderEntry

data PlayerEntry = PlayerEntry
  { _player :: PlayingState
  , _playerHandle :: PlaybackHandle
  }
makeLenses ''PlayerEntry

data BridgeEntry = BridgeEntry
  { _brdgID :: BridgeID
  , _bridgeHandle :: BridgeHandle
  }
makeLenses ''BridgeEntry

type CallsMap     = Map CallID GroupCallEntry
type RecordersMap = Map RecorderID RecorderEntry
type PlayersMap   = Map PlayerID PlayerEntry
type BridgesMap   = Map BridgeID BridgeEntry

data GroupCallAssets=GroupCallAssets
                                  { _calls        :: CallsMap
                                  , _bridges      :: BridgesMap
                                  , _players      :: PlayersMap
                                  , _recorders    ::RecordersMap
                                  }
makeLenses ''GroupCallAssets

newGroupCallAssets :: GroupCallAssets
newGroupCallAssets = GroupCallAssets M.empty M.empty M.empty M.empty

getSnapshot :: GroupCallAssets -> ([GroupCall], [CallRecording], [PlayingState], [BridgeID])
getSnapshot a =
      ( fmap _gcCall      (M.elems  (a ^. calls))
      , fmap _recording   (M.elems  (a ^. recorders))
      , fmap _player      (M.elems  (a ^. players))
      , fmap _brdgID      (M.elems  (a ^. bridges))
      )

getBridges :: GroupCallAssets -> [BridgeEntry]
getBridges a = M.elems  (a ^. bridges)

getBridgeHandles :: GroupCallAssets -> [BridgeHandle]
getBridgeHandles a = fmap _bridgeHandle (M.elems  (a ^. bridges))

getCallIDs :: GroupCallAssets -> [CallID]
getCallIDs a = keys (a^.calls)

type AssetsTransformer = GroupCallAssets -> GroupCallAssets

getBridgeIDs :: GroupCallAssets -> [BridgeID]
getBridgeIDs a = keys (a^.bridges)

getCallEntry :: CallID -> GroupCallAssets -> Maybe GroupCallEntry
getCallEntry callID a = a^.calls ^.at callID

getCall :: CallID -> GroupCallAssets -> Maybe GroupCall
getCall callID a = _gcCall <$> getCallEntry callID a

getBridge :: BridgeID -> GroupCallAssets -> Maybe BridgeEntry
getBridge brdID a = a^.bridges ^.at brdID

getCallChannelHandle :: CallID -> GroupCallAssets -> Maybe ChannelHandle
getCallChannelHandle callID a = _handle <$> getCallEntry callID a

removeCall :: CallID -> AssetsTransformer
removeCall callID = calls %~ delete callID

updateCallEntry :: CallID -> (GroupCallEntry -> GroupCallEntry) -> AssetsTransformer
updateCallEntry callID updateFunc = calls %~ M.adjust updateFunc callID

updateGroupCall :: CallID -> (GroupCall -> GroupCall) -> AssetsTransformer
updateGroupCall callID updateFunc = updateCallEntry callID (gcCall %~ updateFunc)

removeBridge :: BridgeID -> AssetsTransformer
removeBridge brdID = bridges %~ delete brdID
