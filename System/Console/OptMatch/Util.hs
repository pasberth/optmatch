{-# LANGUAGE FlexibleContexts #-}

module System.Console.OptMatch.Util( flag
                                   , unflag
                                   , expect
                                   , shift
                                   , unshift
                                   , just
                                   , anywhere
                                   , include
                                   , extract ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

flag :: Alternative f => f a -> f Bool
flag v = True <$ v <|> pure False

unflag :: Alternative f => f a -> f Bool
unflag v = False <$ v <|> pure True

expect :: (Eq a, Alternative f) => a -> a -> f a
expect p q
  | p == q = pure p
  | otherwise = empty

shift :: (MonadState [a] m, Alternative m) => m a
shift = do
  xs <- get
  case xs of
    [] -> empty
    (x:xs) -> do { put xs; pure x }

unshift :: MonadState [a] m => a -> m [a]
unshift x = modify (x:) >> get

just :: (Eq a, MonadState [a] m, Alternative m) => a -> m a
just a = shift >>= expect a

anywhere :: (MonadState [a] m, Alternative m) =>  m a -> m a
anywhere m = m <|> do
  x <- shift
  a <- anywhere m
  modify (x:)
  return a

include :: (Eq a, MonadState [a] m, Alternative m) => a -> a -> m a
include p q = anywhere $ expect p q

extract :: (Eq a, MonadState [a] m, Alternative m) => a -> m a
extract p = shift >>= (anywhere . expect p)
