{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.MiscUtils (
    transformReaderToStateExcept
  , (.$)
  , (>><)
  , liftIOFunc
  , eitherToMaybe
  , ifJust
  , ifJustMaybe
  , ifJustMap
  , ifJustMapMaybe
  , doWhile
  , doForever
  , mapValues
  , EmptyData (..)
  , IsEmpty (..)
  , NoData (..)
  , ReservedData
  , Never
  ) where

import Protolude
import Control.Lens
-- import Data.Aeson hiding (defaultOptions)
import Elm.Derive
import Data.Map.Strict as M

transformReader::(r2->r1)->ReaderT r1 m a->ReaderT r2 m a
transformReader f rt1 =ReaderT $ runReaderT rt1 . f

transformReaderE::(Monad m)=>(r2->r1)->ReaderT r1 m a->ReaderT r2 m (Either e a)
transformReaderE f rt1 =let rt2= transformReader f rt1
                        in fmap Right rt2

--transformReaderToExcept::(Monad m)=>(r2->r1)->ReaderT r1 m a->ExceptT e (ReaderT r2 m) a
--transformReaderToExcept f rt1 = let rt2E=transformReaderE f rt1
--                                in ExceptT rt2E

transformReaderToStateExcept::(Monad m)=>(r2->r1)->ReaderT r1 m a->ExceptT e (StateT s (ReaderT r2 m)) a
transformReaderToStateExcept f rt1 = let rt2E=transformReaderE f rt1
                                in ExceptT $ lift rt2E

(.$) :: MonadState s m => Lens' s a -> (a-> m b) -> m b
(.$) l f = use l >>= f

liftIOFunc :: (MonadIO m) => (a -> IO b) -> (a -> m b)
liftIOFunc = fmap liftIO

class EmptyData a where
  emptyData :: a
class IsEmpty a where
  isEmpty :: a -> Bool

data NoData = NoData deriving (Eq, Show)
instance EmptyData NoData where
   emptyData = NoData
instance EmptyData () where
 emptyData = ()

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

ifJust :: (Monad m) => m (Maybe a) -> (a-> m b) -> m (Maybe b)
ifJust mma a2mb= mma >>= traverse a2mb

ifJustMaybe :: (Monad m) => m (Maybe a) -> (a-> m (Maybe b)) -> m (Maybe b)
ifJustMaybe mma a2mmb= do
  ma <- mma
  case ma of
    Nothing -> return Nothing
    Just a -> a2mmb a

ifJustMap :: (Monad m) => m (Maybe a) -> (a->b) -> m (Maybe b)
ifJustMap mma a2b= do
  ma <- mma
  return $ fmap a2b ma

ifJustMapMaybe :: (Monad m) => m (Maybe a) -> (a->Maybe b) -> m (Maybe b)
ifJustMapMaybe mma a2mb= do
  ma <- mma
  return $ ma >>= a2mb

data ReservedData = ReservedData deriving (Show, Generic)
--instance ToJSON ReservedData
--instance FromJSON ReservedData
deriveBoth defaultOptions ''ReservedData

doWhile :: (Monad m) => m Bool -> m ()
doWhile loopBody = loopBody >>= (`when` doWhile loopBody)

-- whenM :: (Monad m) => m Bool -> m () -> m ()
-- whenM condM action = condM >>= (flip when) action

f_ :: (Monad m) => a -> (a->m())
f_ _typeParam _ = return ()

(>><) :: (Monad m) => m a -> a -> m ()
(>><) ma a = ma >>= f_ a

--doForever :: Monad m => m () -> m Void
--doForever m = m >> doForever m
type Never = Void
doForever :: (Applicative f) => f a -> f Never
{-# INLINE doForever #-}
doForever a   = let a' = a *> a' in a'

{-
type TP f a b = (Applicative f) => f a -> f b


testForever :: IO ()
testForever = do
  forever $ return ()
  print ("Done"::Text)

testDoForever :: Monad m => m () -> m ()
testDoForever m = doForever m >> m

testDoForever' :: Monad m => m () -> m ()
testDoForever' m = do
  a <- doForever m
  putStrLn (show a)
  m
-}

mapValues :: M.Map k v -> [v]
mapValues mp = snd <$> M.toList mp
