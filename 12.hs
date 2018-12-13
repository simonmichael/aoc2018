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
import Data.CallStack
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

type P = Bool         -- plant
type PS = V.Vector P  -- finite sequence of plants
type Rule = (PS,P)    -- a rule for updating a plant based on its left and right neighbours
type X = Int          -- x coordinate relative to the origin
type Region = (X,PS)  -- a bounded region of plants, starting at the left x coordinate
type G = (Int,Region) -- the nth generation of plants. The region encloses all live plants.
type Score = Int      -- a generation's live plants pot score
type Verbosity = Int  -- verbosity level

neighbourhoodradius = 2

neighbourhoodlength = 2 * neighbourhoodradius + 1

parse :: String -> (G,[Rule])
parse s = (g0, rules)
  where
    l:_:ls = lines s
    g0 = (0, (0, V.fromList $ map toplant $ drop 15 l) )
    rules = map parserule ls

parserule :: String -> Rule
parserule s = (V.fromList $ map toplant ns, toplant p)
  where
    (ns,rest) = splitAt neighbourhoodlength s
    p = last rest

toplant '#' = True
toplant _   = False

fromplant True = '#'
fromplant _    = '.'

-- show this generation's live region (from left-most to right-most live plants).
-- regions are displayed left-aligned, followed by the coordinates of those left-
-- and right-most plants.
showGeneration :: HasCallStack => G -> String
showGeneration (i,(x,ps)) =
  printf "%11d: %4d,%4d  %s" i x (x+l-1) (map fromplant $ V.toList ps)
  where
    l = V.length ps

generationScore :: HasCallStack => G -> Score
generationScore (_,(x,ps)) = sum $ map snd $ filter fst $ zip (V.toList ps) [x..]

-- given a region and an absolute x position that lies within that region,
-- get the neighbourhood-sized region of plants centered at that x position.
-- where the neighbourhood extends beyond the main region, it is filled with empty pots.
getPlants :: HasCallStack => Region -> X -> PS
getPlants (regionleftx,ps) centerx
  | centerx < regionleftx || centerx > regionleftx + V.length ps =
      error $ "getPlants x:"++show centerx++" is outside region"
  | otherwise = 
      let
        leftx     = centerx - neighbourhoodradius
        rightx    = centerx + neighbourhoodradius
        emptypots = V.replicate neighbourhoodradius False
        ps'       = V.concat [emptypots,ps,emptypots]
      in
        V.slice (-- traceWith (\l -> "slice "++show l++"-"++show (l+neighbourhoodlength)) $
                 leftx-regionleftx+neighbourhoodradius) neighbourhoodlength ps'

-- apply rules to decide one plant's next state.
-- arguments: rules, current live region of plants, absolute x position of this plant.
-- returns: this plant's new state.
updatePlant :: HasCallStack => [Rule] -> Region -> X -> P
updatePlant rules r px =
  let
    neighbourhood = getPlants r px
    mp' = lookup neighbourhood rules
  in
    case mp' of
      Just p'  -> p'
      Nothing  -> False

-- | apply rules to generate next generation of plants.
-- may cause the live region to grow or shrink as plants are born or die at the ends.
--
-- >>> g = g0
-- >>> putStrLn $ showGeneration g
--           0:    0,  24  #..#.#..##......###...###
-- >>> g <- nextgen 2 rules g
--           1:    0,  24  #...#....#.....#..#..#..#
-- >>> g <- nextgen 2 rules g
--           2:    0,  25  ##..##...##....#..#..#..##
-- >>> g <- nextgen 2 rules g
--           3:   -1,  25  #.#...#..#.#....#..#..#...#
-- >>> g <- nextgen 2 rules g
--           4:    0,  26  #.#..#...#.#...#..#..##..##
--
nextgen :: HasCallStack => Verbosity -> [Rule] -> Int -> G -> IO G
nextgen verbosity rules maxn (i, oldregion@(x,ps)) =
  let
    l = V.length ps
    emptypots = V.replicate neighbourhoodradius False
    enlargedregion  = (x-neighbourhoodradius, emptypots V.++ ps V.++ emptypots)
    ps' = V.map (updatePlant rules enlargedregion) $ V.fromList [x-neighbourhoodradius .. x+l+neighbourhoodradius-1]
    (newx,newps) = shrinkRegion (x-neighbourhoodradius, ps')
    g' = (i+1, (newx,newps))
  in
    do
      when (verbosity>=2) $ putStrLn $ showGeneration g'
      when (verbosity==1 && i `mod` 1000 == 0) $ putStrLn $ showGeneration g'
      if True -- ps /= newps
      then return g'
      else do
        let
          dx = newx - x
          gensremaining = maxn - (i+1)
          finalx = newx + gensremaining * dx
          finalgen = (maxn, (finalx,newps))
        printf "plants are stable, early exit.\n"
        printf "x changing by %d, %d generations to go, predicted final region start %d\n" dx gensremaining finalx
        return finalgen

-- | shrink the live region to exclude any empty pots at the ends.
shrinkRegion :: Region -> Region
shrinkRegion (x,ps) = (x',ps')
  where
    (leftpots,rest) = V.break id ps
    x'              = x + V.length leftpots
    (rightpots,_)   = V.break id $ V.reverse ps
    ps'             = V.take (V.length rest - V.length rightpots) rest

-- run for n generations, printing each generation or other progress info depending on verbosity level,
-- and returning the last generation's live plants score.
run :: HasCallStack => String -> Int -> Verbosity -> IO Score  -- left-side length, right-side length including 0, initial state and rules input, number of generations to run
run input n verbosity = do
  let (g0,rules) = parse input
  when (verbosity>=2) $ putStrLn $ showGeneration g0
  g <- iterateUntilM ((==n).fst) (nextgen verbosity rules n) g0
  return $ generationScore g

(g0,rules) = parse testinput
iog1 = nextgen 0 rules 20 g0
iog2 = iog1 >>= nextgen 0 rules 20

main :: HasCallStack => IO ()
main = do
  putStrLn "part 1: "
  timeItShow $ run testinput 20 2 -- 325

  putStrLn "\npart 2: "
  args <- getArgs
  let n:verbosity:[] = take 2 $ args ++ ["50000000000","0"]
  timeItShow $ run input (read n) (read verbosity) -- 1700000000011, 0.14s
  return ()

