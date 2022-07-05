module Main where
import qualified System.Random as R
import System.IO
import System.Console.ANSI

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Data.Monoid ((<>))

data Direction = North | South | East | West deriving (Show,Eq)

data Command = Quit | Go Direction deriving (Show,Eq) 

type Position :: (Int,Int)
type Snake :: [Position]

data World = {
    snake :: Snake
  , food :: Position
  , direction :: Direction
  , rand :: R.StdGen
  , limits :: (Int, Int)
} deriving (Show)

data GameState = Playing World | GameOver deriving (Show)

takeUntilAfter :: Monad m => (a -> Bool) -> Pipe a a m ()
takeUntilAfter p = do
  v <- await
  yield v
  if p v then return () else takeUtilAfter p



main :: IO ()
main = putStrLn (titlecase "hello, haskell!")
