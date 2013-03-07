{-# LANGUAGE FlexibleContexts #-}
module System.Console.OptMatch( OptMatch
                              , OptMatchT
                              , runOptMatch
                              , runOptMatchT
                              , flag
                              , unflag
                              , anywhere
                              , keyword
                              , argument ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Maybe

type Args = [String]
type OptMatch = OptMatchT Identity
type OptMatchT m = StateT Args (MaybeT m)

infixr 9 .+
(.+) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .+ g = \a -> f . g a

runOptMatch :: OptMatch a -> Args -> (Maybe (a, Args))
runOptMatch = runIdentity .+ runMaybeT .+ runStateT

runOptMatchT :: Monad m => OptMatchT m a -> Args -> m (Maybe (a, Args))
runOptMatchT = runMaybeT .+ runStateT

flag :: Alternative f => f a -> f Bool
flag v = True <$ v <|> pure False

unflag :: Alternative f => f a -> f Bool
unflag v = False <$ v <|> pure True

shift :: (MonadState [a] m, MonadPlus m) => m a
shift = do
  xs <- get
  case xs of
    [] -> mzero
    (x:xs) -> do { put xs; return x }

unshift :: MonadState [a] m => a -> m ()
unshift x = modify (x:)

just :: (Eq a, MonadState [a] m, MonadPlus m) => a -> m a
just a = do
  x <- shift
  if x == a
    then return a
    else mzero

anywhere :: (MonadState [a] m, MonadPlus m) =>  m a -> m a
anywhere m = mplus m $ do
  x <- shift
  a <- anywhere m
  unshift x
  return a

keyword :: (Eq a, MonadState [a] m, MonadPlus m) => a -> m a
keyword = just

argument :: (MonadState [a] m, MonadPlus m) => m a
argument = shift
