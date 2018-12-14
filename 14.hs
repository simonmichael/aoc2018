#!/usr/bin/env stack
{- stack --resolver=nightly-2018-12-12 script --compile
   --package "ansi-terminal terminal-size containers here megaparsec monad-loops mtl pretty-show safe scanf split time timeit vector"
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
import Control.Concurrent 
import Control.Exception
import Control.Monad.Loops
import Control.Monad.State
import Debug.Trace
import Data.Bifunctor
import Data.CallStack
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List as L
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.String.Here
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector.Unboxed as V
import Debug.Trace
import System.Console.ANSI
import System.Environment
import System.IO
import System.Exit
import System.TimeIt
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf
import qualified Text.Scanf as SC
import Text.Show.Pretty
import System.Console.Terminal.Size

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow

ltrace :: Show a => String -> a -> a
ltrace msg x =
  trace (msg++": "++show x)
  x

iterateN :: Int -> (a -> a) -> (a -> a)
iterateN n f = (!! n) . iterate f   -- n must be positive

first3 (x,_,_) = x
second3 (_,x,_) = x
third3 (_,_,x) = x

first4 (x,_,_,_) = x
second4 (_,x,_,_) = x
third4 (_,_,x,_) = x
fourth4 (_,_,_,x) = x

forceSpine :: [a] -> ()
forceSpine = foldr (const id) ()

forceAllElementsWHNF :: [a] -> ()
forceAllElementsWHNF = foldr seq ()

-- timeIt :: MonadIO m => m a -> m a
-- Wrap a MonadIO computation so that it prints out the execution time.

-- timeItShow :: (MonadIO m, Show a) => m a -> m a
-- Like timeIt, but uses the show rendering of a as label for the timing.

-- timeItNamed :: MonadIO m => String -> m a -> m a
-- Like timeIt, but uses the String as label for the timing.

-- timeItT :: MonadIO m => m a -> m (Double, a)
-- Wrap a MonadIO computation so that it returns execution time in seconds, as well as the result value.

--

t1 = [here|
(3)[7]
 |]
-- (3)[7] 1  0 
--  3  7  1 [0](1) 0 
--  3  7  1  0 [1] 0 (1)
-- (3) 7  1  0  1  0 [1] 2 
--  3  7  1  0 (1) 0  1  2 [4]
--  3  7  1 [0] 1  0 (1) 2  4  5 
--  3  7  1  0 [1] 0  1  2 (4) 5  1 
--  3 (7) 1  0  1  0 [1] 2  4  5  1  5 
--  3  7  1  0  1  0  1  2 [4](5) 1  5  8 
--  3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
--  3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6 
--  3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7 
--  3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7 
--  3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9 
--  3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2
-- 
-- If the Elves think their skill will improve after making 9 recipes, the scores of the ten recipes after the first nine on the scoreboard would be 5158916779 (highlighted in the last line of the diagram).
-- After 5 recipes, the scores of the next ten would be 0124515891.
-- After 18 recipes, the scores of the next ten would be 9251071085.
-- After 2018 recipes, the scores of the next ten would be 5941429882.

type T = Int -- simulation time, 0..
type R = Int -- recipe, 0..9
data W = W { -- world
   wtime :: T
  ,wrecipes :: [R]
  ,welf1 :: Int
  ,welf2 :: Int
} deriving (Show)

parse s =
  let
    rs = chunksOf 3 s
  in
    W{wtime    = 0
     ,wrecipes = map (read.take 1.drop 1) rs
     ,welf1    = fromJust $ findIndex ("(" `isPrefixOf`) rs
     ,welf2    = fromJust $ findIndex ("[" `isPrefixOf`) rs
     }

done n W{..} = wtime == n

step :: (W -> IO W) -> (W -> IO ()) -> (W -> IO W)
step = \update display -> update >=> ((<$) <*> display)

assert' = flip assert (pure ())

main = do
  let w1 = parse t1

  let n = 2
  w' <- iterateUntilM (done n) (step update1 printnothing) w1
  pp $ wrecipes w'
  assert' $ length (wrecipes w') == length (wrecipes w1) + n

  -- input <- parse <$> read "14.in"

update1 w@W{..} = do
  return w{wtime=wtime+1}

printnothing W{..} = pp wtime


-- | get the next n recipes after the given recipe sequence.
-- nextrecipes :: Int -> [R] -> [R]
-- nextrecipes n = take (length rs + n) $ cycle rs 

