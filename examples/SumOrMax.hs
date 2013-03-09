import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import System.Console.OptMatch
import System.Console.OptMatch.Basic
import System.Environment(getArgs)

data Options = Options {
  isSum :: Bool
, numbers :: [Int]
} deriving (Show)

parser :: OptMatch Options
parser = basic defaultOptions $ \opts ->
  opts { isSum = True } <$ keyword "--sum" <|>
  (\n -> opts { numbers = read n : numbers opts }) <$> argument where
    defaultOptions = Options {
      isSum = False
    , numbers = []
      }

main :: IO ()
main = do
  args <- getArgs
  case runOptMatch parser args of
    Just (opts, args) -> let accumulate = if isSum opts then sum else maximum in
      print $ accumulate $ numbers opts
    Nothing -> putStrLn "Usage: [ --sum ] N [N ...]"
