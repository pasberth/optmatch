{-# LANGUAGE FlexibleContexts #-}
module System.Console.OptMatch( OptMatchT
                              , runOptMatchT
                              , flag
                              , unflag
                              , anywhere
                              , keyword
                              , argument ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Maybe

type Args = [String]
type OptMatchT m = StateT Args (MaybeT m)

runOptMatchT :: Monad m => OptMatchT m a -> Args -> m (Maybe (a, Args))
runOptMatchT m = runMaybeT . runStateT m

flag :: Alternative f => f a -> f Bool
flag v = True <$ v <|> pure False

unflag :: Alternative f => f a -> f Bool
unflag v = False <$ v <|> pure True

expect :: (Eq a, Alternative f) => a -> a -> f a
expect p q
  | p == q = pure p
  | otherwise = empty

unexpect :: (Eq a, Alternative f) => a -> a -> f a
unexpect p q
  | p /= q = pure p
  | otherwise = empty

shift :: (MonadState [a] m, MonadPlus m) => m a
shift = do
  xs <- get
  case xs of
    [] -> mzero
    (x:xs) -> do { put xs; return x }

unshift :: MonadState [a] m => a -> m ()
unshift x = modify (x:)

just :: (Eq a, MonadState [a] m, MonadPlus m, Alternative m) => a -> m a
just a = shift >>= expect a

anywhere :: (MonadState [a] m, MonadPlus m) =>  m a -> m a
anywhere m = mplus m $ do
  x <- shift
  a <- anywhere m
  unshift x
  return a

include :: (Eq a, MonadState [a] m, Alternative m, MonadPlus m) => a -> a -> m a
include p q = anywhere $ expect p q

extract :: (Eq a, MonadState [a] m, Alternative m, MonadPlus m) => a -> m a
extract p = shift >>= (anywhere . expect p)

keyword :: (Eq a, MonadState [a] m, MonadPlus m, Alternative m) => a -> m a
keyword = just

argument :: (MonadState [a] m, MonadPlus m) => m a
argument = shift
