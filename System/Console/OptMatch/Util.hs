{-# LANGUAGE FlexibleContexts #-}

module System.Console.OptMatch.Util( flag
                                   , unflag
                                   , expect
                                   , shift
                                   , unshift
                                   , anywhere ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

flag :: Alternative f => f a -> f Bool
flag v = True <$ v <|> pure False

unflag :: Alternative f => f a -> f Bool
unflag v = False <$ v <|> pure True

expect :: (Eq a, MonadPlus m) => a -> m a -> m a
expect p m = do
  q <- m
  if p == q
     then return p
     else mzero

shift :: (MonadState [a] m, MonadPlus m) => m a
shift = do
  xs <- get
  case xs of
    [] -> mzero
    (x:xs) -> do { put xs; return x }

unshift :: MonadState [a] m => a -> m [a]
unshift x = modify (x:) >> get

anywhere :: (MonadState [a] m, MonadPlus m) =>  m a -> m a
anywhere m = mplus m $ do
  x <- shift
  a <- anywhere m
  modify (x:)
  return a
