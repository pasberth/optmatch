module System.Console.OptMatch.Basic where

import System.Console.OptMatch
import System.Console.OptMatch.Prim

keyword :: Monad m => String -> OptMatchT m String
keyword = just

argument :: Monad m => OptMatchT m String
argument = shift

subst :: Monad m => String -> OptMatchT m String
subst = prefix . (++"=")
