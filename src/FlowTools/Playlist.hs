{-# LANGUAGE ExistentialQuantification #-}

module FlowTools.Playlist where

import Protolude hiding (show)
import GHC.Show

import ARICore

class Playable a where
  mediaList :: a -> [PBMedia]

data PlayEntry = forall s. (Playable s, Show s) => PlayEntry s

instance Playable PlayEntry where
   mediaList (PlayEntry entry)= mediaList entry

instance Show PlayEntry where
  show (PlayEntry entry)=show entry

type Playlist = [PlayEntry]

playlist :: Playlist -> [PBMedia]
playlist = concatMap mediaList

data FilePB = FilePB {_pbFileName :: Text} deriving Show
instance Playable FilePB where
  mediaList (FilePB fileName) = [PBSound $ PBSoundParams fileName]

data RecordingPB = RecordingPB {_pbRecordingName :: Text} deriving Show
instance Playable RecordingPB where
  mediaList (RecordingPB recordName) = [PBRecording $ PBRecordingParams recordName]

filePB :: Text -> PlayEntry
filePB = PlayEntry . FilePB
