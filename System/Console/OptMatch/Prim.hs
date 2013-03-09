{-# LANGUAGE FlexibleContexts #-}
module System.Console.OptMatch.Prim where

import Control.Applicative
import Control.Monad.State
import qualified Data.List as L

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

prefix :: (Eq a, MonadState [[a]] m, MonadPlus m) => [a] -> m [a]
prefix pre = do
  stream <- shift
  case L.stripPrefix pre stream of
    Just suf -> do { unshift suf; return stream }
    Nothing -> mzero
