module System.Console.OptMatch.Prim where

import Control.Applicative
import Control.Monad.State
import System.Console.OptMatch
import qualified Data.List as L

flag :: Alternative f => f a -> f Bool
flag v = True <$ v <|> pure False

unflag :: Alternative f => f a -> f Bool
unflag v = False <$ v <|> pure True

shift :: Monad m => OptMatchT m String
shift = do
  xs <- get
  case xs of
    [] -> mzero
    (x:xs) -> do { put xs; return x }

unshift :: Monad m => String -> OptMatchT m ()
unshift x = modify (x:)

just :: Monad m => String -> OptMatchT m String
just a = do
  x <- shift
  if x == a
    then return a
    else mzero

prefix :: Monad m => String -> OptMatchT m String
prefix pre = do
  stream <- shift
  case L.stripPrefix pre stream of
    Just suf -> do { unshift suf; return stream }
    Nothing -> mzero
