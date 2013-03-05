module System.Console.OptMatch.Matcher where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Maybe

type MatcherT s m = StateT s (MaybeT m)

runMatcherT :: Monad m => StateT s (MaybeT m) a -> s -> (m (Maybe (a, s)))
runMatcherT m = runMaybeT . runStateT m
