{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module ChannelInState  where

import Protolude
import Control.Exception (evaluate)
import Control.Monad

import Utils
import ARICore

data ObjState = ObjState1 | ObjState12 deriving (Show, Eq)
data Object = Object ObjState deriving (Show, Eq)

findObjectInState :: [Object] -> ObjState -> Maybe Object
findObjectInState objects state = undefined

objs=[Object ObjState1, Object ObjState12]
objinState1= findObjectInState objs ObjState1

-- want to cause compile error of trying to pass Object not at ObjState1
doSomethingWithObjInState1 :: Object -> IO ()
doSomethingWithObjInState1 o1@(Object ObjState1) = print o1
doSomethingWithObjInState1 _ =  print "Unexpected"
