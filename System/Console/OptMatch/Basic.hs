module System.Console.OptMatch.Basic where

import Control.Applicative
import Control.Monad.State
import System.Console.OptMatch
import System.Console.OptMatch.Types
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

just :: (Monad m, ArgType a) => a -> OptMatchT m a
just a = do
  x <- shift
  if x == toArg a
    then return a
    else mzero

prefix :: (Monad m, ArgType a) => a -> OptMatchT m ()
prefix pre = do
  stream <- shift
  case L.stripPrefix (toArg pre) stream of
    Just suf -> unshift suf
    Nothing -> mzero

subst :: (Monad m, ArgType a) => a -> OptMatchT m ()
subst = prefix . (++"=") . toArg

integer :: Monad m => OptMatchT m Integer
integer = do
  x <- shift
  if all C.isDigit x
    then return $ read x
    else mzero

choices :: (Monad m, ArgType a) => [a] -> OptMatchT m a
choices l = do
  x <- shift
  foldl (\a b -> mplus a (if toArg b == x then return b else mzero)) mzero l
