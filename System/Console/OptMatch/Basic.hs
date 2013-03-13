module System.Console.OptMatch.Basic where

import Control.Applicative
import Control.Monad.State
import System.Console.OptMatch
import qualified Data.Char as C
import qualified Data.List as L

flag :: Alternative f => f a -> f Bool
flag v = True <$ v <|> pure False

unflag :: Alternative f => f a -> f Bool
unflag v = False <$ v <|> pure True

unexpect :: MonadPlus m => m a -> m ()
unexpect m = do
  x <- mplus (m >> return True) (return False)
  if x then mzero else return ()

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

subst :: Monad m => String -> OptMatchT m String
subst = prefix . (++"=")

integer :: Monad m => OptMatchT m Integer
integer = do
  x <- shift
  if all C.isDigit x
    then return $ read x
    else mzero

choices :: Monad m => [String] -> OptMatchT m String
choices l = do
  x <- shift
  if any (==x) l
    then return x
    else mzero
