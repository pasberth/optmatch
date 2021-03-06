import Control.Applicative
import System.Console.OptMatch
import System.Console.OptMatch.Basic
import System.Console.OptMatch.Popular
import System.Environment(getArgs)

data Options = Options {
  isSum :: Bool
, numbers :: [Integer]
} deriving (Show)

parser :: OptMatch Options
parser = popular defaultOptions $ \opts ->
  opts { isSum = True } <$ keyword "--sum" <|>
  (\n -> opts { numbers = n : numbers opts }) <$> integer where
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
