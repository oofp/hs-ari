{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module MapsContainer  where

import Data.Map
import Control.Lens
import Protolude

type ObjMap a = Map Int a

class HasObjMap s a where
  hasMap :: Lens' s (ObjMap a)


data TwoObjContainer = TwoObjContainer {_iMap :: ObjMap Int, _bMap :: ObjMap Bool}
makeLenses ''TwoObjContainer

--instance (HasObjMap TwoObjContainer Int) TwoObjContainer
instance HasObjMap TwoObjContainer Int where hasMap = iMap

instance HasObjMap TwoObjContainer Bool where hasMap = bMap
