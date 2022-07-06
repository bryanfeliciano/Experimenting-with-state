module Main where

-- Import statements --

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

-- Data Types --

data Direction = North | South | East | West deriving (Show,Eq)

data Command = Quit | Go Direction deriving (Show,Eq) 

type Position = (Int,Int)
type Snake = [Position]

data World = World {
    snake :: Snake
  , food :: Position
  , direction :: Direction
  , rand :: R.StdGen
  , limits :: (Int, Int)
} deriving (Show)

data GameState = Playing World | GameOver deriving (Show)

-- Logic --

takeUntilAfter :: Monad m => (a -> Bool) -> Pipe a a m ()
takeUntilAfter p = do
  v <- await
  yield v
  if p v then return () else takeUtilAfter p

deltas :: Monad m => Pipe a (a,a) m ()
deltas = do
    first <- await
    P.scan remember (first, first) id
    where
        remember (_, a) b = (a, b)

rateLimit :: Int -> Pipe b b IO ()
rateLimit t = forever $ do
    lift $ threadDelay (t * 100000)
    await >>= yield

opposite :: Direction -> Direction
opposite d = case d of
    North -> South
    South -> North
    East -> West
    West -> East

move :: Direction -> Position -> Position
move d (r, c) = case d of
    North -> (r - 1, c)
    South -> (r + 1, c)
    East -> (r, c + 1)
    West -> (r, c - 1)

slither :: Snake -> Direction -> Snake
slither s d = (move d $ head s):(init s)

eat :: Snake -> Direction -> Snake
eat s d = (move d $ head s):s

randomPosition :: R.RandomGen g => (Int, Int) -> g -> (Position, g)
randomPosition (maxr, maxc) g =
    let (r, g1) = R.randomR (1, maxr) g
        (c, g2) = R.randomR (1, maxc) g1
    in ((r, c), g2)

randomFreePosition :: R.RandomGen g => (Int, Int) -> g -> Snake -> (Position, g)
randomFreePosition lim g s =
    head $ dropWhile inSnake (randomPositions g)
    where inSnake (x, _) = x `elem` s
          randomPositions h = r:randomPositions g'
              where r@(_, g') = randomPosition lim h


advance :: World -> Direction -> World
advance w newDir
    | newDir == opposite (direction w) = w
    | (move newDir $ head $ snake w) == (food w) = eaten
    | otherwise = slithered
    where slithered = w { snake = slither (snake w) newDir
                        , direction = newDir
                        }
          eaten = w { snake = eat (snake w) newDir
                    , direction = newDir
                    , food = newFood
                    , rand = newRand
                    }
          (newFood, newRand) = randomFreePosition (limits w) (rand w) $ snake eaten

toGameState :: World -> GameState
toGameState w
    | collision $ snake w = GameOver
    | any (outside $ limits w) (snake w) = GameOver
    | otherwise = Playing w
    where
        collision (x:xs) = any (== x) (tail xs)
        outside (maxr, maxc) (r, c) =
            r < 1 || r > maxr || c < 1 || c > maxc

transitions game =
    P.scan advance game id
    >-> P.map toGameState
    >-> takeUntilAfter isGameOver
    >-> deltas
    where
        isGameOver GameOver = True
        isGameOver (Playing _) = False

-- Input --



main :: IO ()
main = putStrLn ("hello, haskell!")
