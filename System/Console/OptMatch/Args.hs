module System.Console.OptMatch.Args where

import Control.Applicative
import Control.Monad.State
import System.Console.OptMatch.Util
import System.Console.OptMatch.Matcher

type Args = [String]

keyword :: (Functor m, Monad m) => String -> MatcherT Args m String
keyword = just

argument :: (Functor m, Monad m) => MatcherT Args m String
argument = shift
