module ARITestEnv where

import Protolude
import Prelude (String)
import Control.Concurrent.STM.TVar

import ARICore
import FlowTools
import Utils

srvAddress::String
--srvAddress="192.168.56.101"
--srvAddress="192.168.2.72"
srvAddress="192.168.2.66"

applName::String
applName="hello-world"

config :: ARIConfig
config = ARIConfig applName srvAddress "boris" "boris" 8088 

newAriEnv :: IO ARIEnv
newAriEnv = initARI config undefined -- newChannelHandler

setState :: s -> FlowContext s ()
setState s = lift $ put s

appendState :: (Monoid s) => s -> FlowContext s ()
appendState suffix = lift $ modify (<> suffix)

appendStateF :: (Monoid s, Applicative f) => f s -> FlowContext (f s) ()
appendStateF suffix = lift $ modify (\s-> liftA2 (<>) s suffix)

setStateAndExit :: s -> FlowContext s ()
setStateAndExit st = setState st >> exitFlow

appendStateAndExit :: s -> FlowContext s ()
appendStateAndExit st = setState st >> exitFlow

appendStateFAndExit :: (Monoid s, Applicative f) => f s -> FlowContext (f s) ()
appendStateFAndExit st = appendStateF st >> exitFlow

runFlowAndExit :: FlowCallback s s -> FlowContext s ()
runFlowAndExit flowCallback = void $ runCall flowCallback setStateAndExit

runFlowAppendStateAndExit :: FlowCallback s s -> FlowContext s ()
runFlowAppendStateAndExit flowCallback = void $ runCall flowCallback appendStateAndExit

appendStateCB :: (Monoid s) => s -> FlowCallback s ()
appendStateCB  = lift . appendState

appendToTVar :: (Monoid t, MonadIO m) => TVar t -> t -> m ()
appendToTVar tvar t = liftIO $ atomically $ modifyTVar tvar (<>t)

runAndCancel :: FlowCallback s a -> FlowContext s ()
runAndCancel  flowCallback = do
  cc <- runCall flowCallback (\_->return ())
  cancelCall cc
