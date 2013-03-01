import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import OptMatch
import System.Environment(getArgs)

data Options = Options {
  isSum :: Bool
, numbers :: [Int]
} deriving (Show)

parser :: Monad m => MatcherT Args m Options
parser = do
  isSum <- flag $ anywhere $ keyword "--sum"
  ns <- some (read <$> argument)
  return (Options isSum ns)

main :: IO ()
main = do
  args <- getArgs
  a <- match parser args
  case a of
    Just opts -> let accumulate = if isSum opts then sum else maximum in
      print $ accumulate $ numbers opts
    Nothing -> putStrLn "Usage: [ --sum ] N [N ...]"
