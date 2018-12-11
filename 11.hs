#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package "ansi-terminal containers here megaparsec mtl pretty-show safe scanf split time"
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
import Prelude hiding (reverse,length)
import Control.Monad.State
import Debug.Trace
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable hiding (length)
import Data.Function
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Sequence as S
import Data.String.Here
import Data.Time.Calendar
import Data.Time.Clock
import Debug.Trace
import System.Console.ANSI
import System.Environment
import System.IO
import System.Exit
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf
import qualified Text.Scanf as SC
import Text.Show.Pretty

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



type X = Int    -- x position
type Y = Int    -- y position
type P = Int    -- power level
type S = Int    -- size of square of cells or square grid
type G = Seq (Seq P)  -- grid of cell power levels

-- | Calculate power level of the cell at x y given serial number s.
--
-- #>>> cellpower 8 3 5
-- 4
--
-- #>>> cellpower 57 122 79
-- -5
--
-- #>>> cellpower 39 217 196
-- 0
--
-- #>>> cellpower 71 101 153
-- 4
cellpower :: Int -> X -> Y -> P
cellpower s x y =
  let rid = x + 10
  in
    read ([L.reverse (show ((rid * y + s) * rid)) !! 2]) - 5

-- | Make a grid of n x n cell power levels based on serial number s.
grid :: Int -> Int -> G
grid s n =
  fromList [fromList [cellpower s x y | x <- [1..n]] | y <- [1..n]]

g3 = grid 0 3
g5 = grid 10 5

g18 = grid 18 300
g42 = grid 42 300
g   = grid 1308 300

-- Look up power level of cell at 1-based x,y in grid g.
cellat :: G -> X -> Y -> P
cellat g x y = g `index` (y-1) `index` (x-1)

-- | Find power level of the 3 x 3 cells whose top left is x y in grid g.
--
-- #>>> let g = g18 in uncurry (power3 g) (locate3 g)
-- 29
--
-- #>>> let g = g42 in uncurry (power3 g) (locate3 g)
-- 30
power3 :: G -> X -> Y -> P
power3 g tlx tly =
  sum $ concat [[cellat g x y | x <- [tlx..tlx+2]] | y <- [tly..tly+2]]

-- | Find top left coordinate of the 3 x 3 cells with highest power level in grid g.
--
-- #>>> locate3 g3
-- (1,1)
--
-- #>>> locate3 g18
-- (33,45)
--
-- #>>> locate3 g42
-- (21,61)
locate3 :: G -> (X,Y)
locate3 g =
  let
    maxx = length (g `index` 0) - 2
    maxy = length g - 2
  in
    snd $ maximumBy (comparing fst) $
    concat [[(power3 g x y,(x,y)) | x <- [1..maxx]] | y <- [1..maxy]]

-- | Find power level of the n x n cells whose top left is x y in grid g.
--
-- #>>> let g = g18 in uncurry (powern g n) (locate g)
-- 29
--
-- #>>> let g = g42 in uncurry (powern g n) (locate g)
-- 30
powern :: G -> S -> X -> Y -> P
powern g n tlx tly =
  sum $ concat [[cellat g x y | x <- [tlx..tlx+(n-1)]] | y <- [tly..tly+(n-1)]]

-- | Find power and top left coordinate of the n x n cells with highest power level in grid g.
--
-- #>>> locaten g3 3
-- (-23,(1,1))
--
locaten :: G -> S -> (P,(X,Y))
locaten g n =
  let
    maxx = length (g `index` 0) - (n-1)
    maxy = length g - (n-1)
  in
    maximumBy (comparing fst) $
    concat [[(powern g n x y,(x,y)) | x <- [1..maxx]] | y <- [1..maxy]]

-- | Find top left coordinate and size of the cell square with highest power level in grid g.
--
-- #>>> locate g3
-- ((0,(3,3)),1)
--
-- #>>> locate g18
-- ((113,(90,269)),16)
locate :: G -> ((P,(X,Y)),S)
locate g =
  let
    maxn = length g
  in
    maximumBy (comparing (fst.fst)) $ [(locaten g (ltrace "comparing" n), n) | n <- [1..maxn]]

main = do
  pp $ locate3 g -- 21,41
  pp $ locate g
