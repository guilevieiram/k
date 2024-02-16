module Monads where

import Control.Monad.State
import Control.Monad.Trans.Maybe

type MaybeStateIO s a = MaybeT (StateT s IO) a

liftFromState :: (Monad m) => m a -> MaybeT m a
liftFromState = lift

liftFromVariable :: a -> MaybeStateIO s a
liftFromVariable = liftFromState . lift . returnIO

liftFromIO :: IO a -> MaybeStateIO s a
liftFromIO = liftFromState . lift

liftFromMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftFromMaybe = MaybeT . return

returnIO :: a -> IO a
returnIO = return
