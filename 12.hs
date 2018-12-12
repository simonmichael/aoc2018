#!/usr/bin/env stack
{- stack --resolver=nightly-2018-12-12 script --compile
   --package "ansi-terminal containers here megaparsec monad-loops mtl pretty-show safe scanf split time timeit vector"
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
import Control.Monad.Loops
import Control.Monad.State
import Debug.Trace
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
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
import System.IO.Unsafe
import System.Exit
import System.TimeIt
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

-- | Trace (print to stderr) a showable value using a custom show function.
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

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

--

testinput = [here|
initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
|]

--               1         2         3     
--     0         0         0         0     
testoutput = [here|
 0: ...#..#.#..##......###...###...........
 1: ...#...#....#.....#..#..#..#...........
 2: ...##..##...##....#..#..#..##..........
 3: ..#.#...#..#.#....#..#..#...#..........
 4: ...#.#..#...#.#...#..#..##..##.........
 5: ....#...##...#.#..#..#...#...#.........
 6: ....##.#.#....#...#..##..##..##........
 7: ...#..###.#...##..#...#...#...#........
 8: ...#....##.#.#.#..##..##..##..##.......
 9: ...##..#..#####....#...#...#...#.......
10: ..#.#..#...#.##....##..##..##..##......
11: ...#...##...#.#...#.#...#...#...#......
12: ...##.#.#....#.#...#.#..##..##..##.....
13: ..#..###.#....#.#...#....#...#...#.....
14: ..#....##.#....#.#..##...##..##..##....
15: ..##..#..#.#....#....#..#.#...#...#....
16: .#.#..#...#.#...##...#...#.#..##..##...
17: ..#...##...#.#.#.#...##...#....#...#...
18: ..##.#.#....#####.#.#.#...##...##..##..
19: .#..###.#..#.#.#######.#.#.#..#.#...#..
20: .#....##....#####...#######....#.#..##.
|]

input = [here|
initial state: ###....#..#..#......####.#..##..#..###......##.##..#...#.##.###.##.###.....#.###..#.#.##.#..#.#

..### => #
..... => .
..#.. => .
.###. => .
...## => #
#.### => .
#.#.# => #
##..# => .
##.## => #
#...# => .
..##. => .
##.#. => .
...#. => .
#..#. => #
.#### => #
.#..# => #
##... => #
.##.# => .
....# => .
#.... => .
.#.#. => #
.##.. => .
###.# => #
####. => .
##### => #
#.##. => #
.#... => #
.#.## => #
###.. => #
#..## => .
#.#.. => #
..#.# => .
|]

type P = Bool        -- plant
type PS = V.Vector P -- finite sequence of plants
type N = PS          -- a plant and its left and right neighbours
type R = (N,P)       -- a rule for updating a neighbourhood's center plant
type G = (Int,PS)    -- the nth generation of (a finite extent of) all plants

neighbourhoodradius = 2
neighbourhoodlength = 2 * neighbourhoodradius + 1

parse :: Int -> Int -> String -> (G,[R])
parse llength rlength s = (g0, rules)
  where
    l:_:ls = lines s
    virtualendneighbours = V.replicate neighbourhoodradius False
    g0 = (0, V.concat [
       virtualendneighbours
      ,V.replicate llength False
      ,V.fromList $ take (rlength+1) $ map toplant $ drop 15 l ++ repeat '.'
      ,virtualendneighbours
      ])
    rules = map parserule ls

parserule :: String -> R
parserule s = (V.fromList $ map toplant ns, toplant p)
  where
    (ns,rest) = splitAt neighbourhoodlength s
    p = last rest

toplant '#' = True
toplant _   = False

fromplant True = '#'
fromplant _    = '.'

showGeneration :: G -> String
showGeneration (i,ps) = printf "%10d: " i ++ (map fromplant $ drop 2 $ init $ init $ V.toList ps)

generationScore :: Int -> G -> Int
generationScore llength (_,ps) =
  sum $ map snd $ filter fst $
  zip (drop 2 $ init $ init $ V.toList ps) [-llength..]

-- apply rules to decide one plant's next state
apply :: [R] -> PS -> Int -> P -> P  -- rules, all plants, index of this plant, this plant's old state
apply rules ps i p
  | i < neighbourhoodradius || i > V.length ps - (neighbourhoodradius+1) = False
  | otherwise = 
      let
        n = V.slice (i-neighbourhoodradius) neighbourhoodlength ps
        mp' = lookup n rules
      in
        case mp' of
          Just p'  -> p'
          Nothing  -> False

-- apply rules to generate next generation of plants
nextgen rules (i,ps) =
  let
    ps' = V.imap (apply rules ps) ps
    g'  = (i+1, ps')
    l   = V.length ps'
  in
    if False then undefined -- any (ps' V.!) [2,3,l-2,l-3] then do
      -- putStrLn $ showGeneration g'
      -- return (error $ "buffer overflow at generation "++show (i+1)
    else do
      when (i `mod` 1 == 0) $ putStrLn $ showGeneration g'
      return g'
      
-- part1 = do
--   putStrLn $ replicate (12+llength) ' ' ++ "          1         2         3         4"
--   putStrLn $ replicate (12+llength) ' ' ++ "0         0         0         0         0"
--   let gs = take 21 $ iterate' (nextgen rules) $ traceWith showGeneration g0
--   -- mapM_ (putStrLn.showGeneration) gs
--   pp $ generationScore $ last gs  -- 2040

-- run for n generations, printing each generation or other progress info depending on verbosity level,
-- and returning the last generation's live plants score.
run :: String -> Int -> Int -> Int -> Int -> IO Int  -- left-side length, right-side length including 0, initial state and rules input, number of generations to run
run input llength rlength verbosity n = do
  putStrLn $ replicate (12+llength) ' ' ++ "          1         2         3         4"
  putStrLn $ replicate (12+llength) ' ' ++ "0         0         0         0         0"
  let (g0,rules) = parse llength rlength input
  when (verbosity>=2) $ putStrLn $ showGeneration g0
  g <- iterateUntilM ((==n).fst) (nextgen rules) g0
  return $ generationScore llength g

main = do
  hSetBuffering stdout NoBuffering
  -- part1
  run testinput 5 40 1 20 >>= pp -- 325
  -- part2
  -- run input 10 1000 0 5000 >>= pp
  -- run input 10000 10000 0 50000000000


-- timeIt :: MonadIO m => m a -> m a
-- Wrap a MonadIO computation so that it prints out the execution time.

-- timeItShow :: (MonadIO m, Show a) => m a -> m a
-- Like timeIt, but uses the show rendering of a as label for the timing.

-- timeItNamed :: MonadIO m => String -> m a -> m a
-- Like timeIt, but uses the String as label for the timing.

-- timeItT :: MonadIO m => m a -> m (Double, a)
-- Wrap a MonadIO computation so that it returns execution time in seconds, as well as the result value.
