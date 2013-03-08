{-# LANGUAGE FlexibleContexts #-}
module System.Console.OptMatch( OptMatch
                              , OptMatchT
                              , runOptMatch
                              , runOptMatchT
                              , flag
                              , unflag
                              , keyword
                              , argument
                              , prefix
                              , basic ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Maybe
import qualified Data.List as L

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

keyword :: (Eq a, MonadState [a] m, MonadPlus m) => a -> m a
keyword = just

argument :: (MonadState [a] m, MonadPlus m) => m a
argument = shift

prefix :: (Eq a, MonadState [[a]] m, MonadPlus m) => [a] -> m [a]
prefix pre = do
  stream <- shift
  case L.stripPrefix pre stream of
    Just suf -> do { unshift suf; return stream }
    Nothing -> mzero

basic :: (Functor m, Monad m) => a -> (a -> OptMatchT m a) -> OptMatchT m a
basic def m = f def where
  f a = do
    r <- m a
    basic r m <|> return r
