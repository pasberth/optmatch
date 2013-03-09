module System.Console.OptMatch( OptMatch
                              , OptMatchT
                              , runOptMatch
                              , runOptMatchT ) where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Maybe
import System.Console.OptMatch.Types

type OptMatch = OptMatchT Identity
type OptMatchT m = StateT Args (MaybeT m)

infixr 9 .+
(.+) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .+ g = \a -> f . g a

runOptMatch :: OptMatch a -> Args -> (Maybe (a, Args))
runOptMatch = runIdentity .+ runMaybeT .+ runStateT

runOptMatchT :: Monad m => OptMatchT m a -> Args -> m (Maybe (a, Args))
runOptMatchT = runMaybeT .+ runStateT
