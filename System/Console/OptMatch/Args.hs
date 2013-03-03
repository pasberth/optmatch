module System.Console.OptMatch.Args where

import Control.Applicative
import Control.Monad.State
import System.Console.OptMatch.Util
import System.Console.OptMatch.Matcher

type Args = [String]

keyword :: Monad m => String -> MatcherT Args m String
keyword = just

argument :: Monad m => MatcherT Args m String
argument = shift
