#!/usr/bin/env stack
{- stack --resolver=nightly-2018-12-12 script --compile
   --package ansi-terminal
   --package call-stack
   --package containers
   --package here
   --package megaparsec
   --package monad-loops
   --package mtl
   --package pretty-show
   --package safe
   --package scanf
   --package split
   --package terminal-size
   --package time
   --package timeit
   --package vector
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
import qualified Data.Array as A
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

-- assert that prints to stdout in normal output sequence
assert_ :: (HasCallStack, MonadIO m) => Bool -> m ()
assert_ True  = return ()
assert_ False = liftIO $ putStrLn ": wrong" >> exitFailure

-- | find starting index of the first occurence of the first sequence within the second
seqIndexL :: Eq a => S.Seq a -> S.Seq a -> Maybe Int
seqIndexL needle haystack =
  findIndex (\t -> S.take (S.length needle) t == needle) $ toList $ S.tails haystack

-- timeIt :: MonadIO m => m a -> m a
-- Wrap a MonadIO computation so that it prints out the execution time.

-- timeItShow :: (MonadIO m, Show a) => m a -> m a
-- Like timeIt, but uses the show rendering of a as label for the timing.

-- timeItNamed :: MonadIO m => String -> m a -> m a
-- Like timeIt, but uses the String as label for the timing.

-- timeItT :: MonadIO m => m a -> m (Double, a)
-- Wrap a MonadIO computation so that it returns execution time in seconds, as well as the result value.

-- examples

-- Targets:      In range:     Reachable:    Nearest:      Chosen:
-- #######       #######       #######       #######       #######
-- #E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
-- #...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
-- #.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
-- #######       #######       #######       #######       #######

t1 = [here|
#######
#E..G.#
#...#.#
#.G.#G#
#######
|]

-- In range:     Nearest:      Chosen:       Distance:     Step:
-- #######       #######       #######       #######       #######
-- #.E...#       #.E...#       #.E...#       #4E212#       #..E..#
-- #...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
-- #..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
-- #######       #######       #######       #######       #######

-- Initially:
-- #########
-- #G..G..G#
-- #.......#
-- #.......#
-- #G..E..G#
-- #.......#
-- #.......#
-- #G..G..G#
-- #########

-- After 1 round:
-- #########
-- #.G...G.#
-- #...G...#
-- #...E..G#
-- #.G.....#
-- #.......#
-- #G..G..G#
-- #.......#
-- #########

-- After 2 rounds:
-- #########
-- #..G.G..#
-- #...G...#
-- #.G.E.G.#
-- #.......#
-- #G..G..G#
-- #.......#
-- #.......#
-- #########

-- After 3 rounds:
-- #########
-- #.......#
-- #..GGG..#
-- #..GEG..#
-- #G..G...#
-- #......G#
-- #.......#
-- #.......#
-- #########

--        HP:            HP:
-- G....  9       G....  9  
-- ..G..  4       ..G..  4  
-- ..EG.  2  -->  ..E..     
-- ..G..  2       ..G..  2  
-- ...G.  1       ...G.  1

-- Initially:
-- #######   
-- #.G...#   G(200)
-- #...EG#   E(200), G(200)
-- #.#.#G#   G(200)
-- #..G#E#   G(200), E(200)
-- #.....#   
-- #######   

-- After 1 round:
-- #######   
-- #..G..#   G(200)
-- #...EG#   E(197), G(197)
-- #.#G#G#   G(200), G(197)
-- #...#E#   E(197)
-- #.....#   
-- #######   

-- After 2 rounds:
-- #######   
-- #...G.#   G(200)
-- #..GEG#   G(200), E(188), G(194)
-- #.#.#G#   G(194)
-- #...#E#   E(194)
-- #.....#   
-- #######   

-- Combat ensues; eventually, the top Elf dies:

-- After 23 rounds:
-- #######   
-- #...G.#   G(200)
-- #..G.G#   G(200), G(131)
-- #.#.#G#   G(131)
-- #...#E#   E(131)
-- #.....#   
-- #######   

-- After 24 rounds:
-- #######   
-- #..G..#   G(200)
-- #...G.#   G(131)
-- #.#G#G#   G(200), G(128)
-- #...#E#   E(128)
-- #.....#   
-- #######   

-- After 25 rounds:
-- #######   
-- #.G...#   G(200)
-- #..G..#   G(131)
-- #.#.#G#   G(125)
-- #..G#E#   G(200), E(125)
-- #.....#   
-- #######   

-- After 26 rounds:
-- #######   
-- #G....#   G(200)
-- #.G...#   G(131)
-- #.#.#G#   G(122)
-- #...#E#   E(122)
-- #..G..#   G(200)
-- #######   

-- After 27 rounds:
-- #######   
-- #G....#   G(200)
-- #.G...#   G(131)
-- #.#.#G#   G(119)
-- #...#E#   E(119)
-- #...G.#   G(200)
-- #######   

-- After 28 rounds:
-- #######   
-- #G....#   G(200)
-- #.G...#   G(131)
-- #.#.#G#   G(116)
-- #...#E#   E(113)
-- #....G#   G(200)
-- #######   

-- More combat ensues; eventually, the bottom Elf dies:

-- After 47 rounds:
-- #######   
-- #G....#   G(200)
-- #.G...#   G(131)
-- #.#.#G#   G(59)
-- #...#.#   
-- #....G#   G(200)
-- #######   

-- Before the 48th round can finish, the top-left Goblin finds that there are no targets remaining, and so combat ends. So, the number of full rounds that were completed is 47, and the sum of the hit points of all remaining units is 200+131+59+200 = 590. From these, the outcome of the battle is 47 * 590 = 27730.

-- Here are a few example summarized combats:

-- #######       #######
-- #G..#E#       #...#E#   E(200)
-- #E#E.E#       #E#...#   E(197)
-- #G.##.#  -->  #.E##.#   E(185)
-- #...#E#       #E..#E#   E(200), E(200)
-- #...E.#       #.....#
-- #######       #######

-- Combat ends after 37 full rounds
-- Elves win with 982 total hit points left
-- Outcome: 37 * 982 = 36334

-- #######       #######   
-- #E..EG#       #.E.E.#   E(164), E(197)
-- #.#G.E#       #.#E..#   E(200)
-- #E.##E#  -->  #E.##.#   E(98)
-- #G..#.#       #.E.#.#   E(200)
-- #..E#.#       #...#.#   
-- #######       #######   

-- Combat ends after 46 full rounds
-- Elves win with 859 total hit points left
-- Outcome: 46 * 859 = 39514

-- #######       #######   
-- #E.G#.#       #G.G#.#   G(200), G(98)
-- #.#G..#       #.#G..#   G(200)
-- #G.#.G#  -->  #..#..#   
-- #G..#.#       #...#G#   G(95)
-- #...E.#       #...G.#   G(200)
-- #######       #######   

-- Combat ends after 35 full rounds
-- Goblins win with 793 total hit points left
-- Outcome: 35 * 793 = 27755

-- #######       #######   
-- #.E...#       #.....#   
-- #.#..G#       #.#G..#   G(200)
-- #.###.#  -->  #.###.#   
-- #E#G#G#       #.#.#.#   
-- #...#G#       #G.G#G#   G(98), G(38), G(200)
-- #######       #######   

-- Combat ends after 54 full rounds
-- Goblins win with 536 total hit points left
-- Outcome: 54 * 536 = 28944

-- #########       #########   
-- #G......#       #.G.....#   G(137)
-- #.E.#...#       #G.G#...#   G(200), G(200)
-- #..##..G#       #.G##...#   G(200)
-- #...##..#  -->  #...##..#   
-- #...#...#       #.G.#...#   G(200)
-- #.G...G.#       #.......#   
-- #.....G.#       #.......#   
-- #########       #########   

-- Combat ends after 20 full rounds
-- Goblins win with 937 total hit points left
-- Outcome: 20 * 937 = 18740

--

-- outcome is
--  number of full rounds that were completed (not counting the round in which combat ends)
--  multiplied by the sum of the hit points of all remaining units at the moment combat ends. (Combat only ends when a unit finds no targets during its turn.)
-- What is the outcome of the combat described in your puzzle input?

-- data

type T = Int -- simulation time, 0..
data W = W { -- simulation world
   wtime    :: T
  ,wmap     :: A.Array Pos Tile
  ,wunits   :: [U]
} deriving (Eq,Show)
data Tile = Wall | Floor deriving (Eq,Show)
data U = U {
   utype    :: Type
  ,upos     :: Pos
  ,uhp      :: HP
} deriving (Eq,Show)
data Type = E | G deriving (Eq,Show)  -- elf, goblin
type X = Int
type Y = Int
type Pos = (X,Y)
type HP = Int

parse :: String -> W
parse s =
  let
    ls = lines s
    h  = length ls
    w  = length $ head ls
  in
    W{wtime   = 0
     ,wmap    = A.array ((0,0),(w-1,h-1)) [ ((x,y), parsetile c) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l ]
     ,wunits  = catMaybes $ concat [ [(parseunit x y c) | (x,c) <- zip [0..] l ] | (y,l) <- zip [0..] ls ]
    }

parsetile '#' = Wall
parsetile _   = Floor

showtile Wall  = '#'
showtile Floor = '.'

parseunit x y 'E' = Just $ U {utype=E, upos=(x,y), uhp=defhp}
parseunit x y 'G' = Just $ U {utype=G, upos=(x,y), uhp=defhp}
parseunit _ _ _   = Nothing

showunit U{..} = head $ show utype

defhp = 200

-- main

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- let (usage,defargs) = ("Usage: ./14 [INPUTFILE]", ["14.in"])
  -- args <- getArgs
  -- -- when (null args) $ putStrLn usage >> exitSuccess
  -- -- let [f] = take 1 $ args ++ drop (length args) defargs
  -- -- input <- parse <$> readFile f

  -- part 1
  (t,w) <- timeItT $ iterateUntilM ((==2).wtime) (update >=> printworld) <=< printworld $ parse t1
  printf "\n%.3fs to simulate %d ticks (%.0f ticks/s)\n" t (wtime w) (fromIntegral (wtime w) / t)

  -- part 2
  -- (t,w) <- timeItT $ iterateUntilM ((==1000).wtime) (update2 >=> printworld) $ parse t1
  -- printf "%.3fs to simulate %d ticks (%.0f ticks/s)\n" t (wtime w) (fromIntegral (wtime w) / t)
  -- let x = wx w
  -- putStr x >> assert_ (x == 123) >> putStrLn ": ok"

-- update

update :: W -> IO W
update w@W{..} = do
  -- "Regardless of how the unit's turn ends, the next unit in the round takes its turn."
  wunits' <- mapM (updateunit w) $ sortunits wunits
  return w{
     wtime   = wtime + 1
    ,wunits  = wunits'
    }

sortunits = sortOn (\U{upos=(x,y)} -> (y,x))

updateunit :: W -> U -> IO U
updateunit w@W{..} u@U{..} = do
  -- move
  let targets = filter (u `doestarget`) wunits
  let inrange = filter (isinrange u) targets 
  --  if not target-adjacent, find most reachable target-adjacent space
  let mdest = case inrange of
          []  -> Just $ minimumBy (comparing (distance upos)) $
                        concatMap (filter (isempty w) . adjacentspaces w) targets
          _   -> Nothing
    --   move towards
  -- attack
  --  if target-adjacent
  --   select lowest-hp adjacent target
  --   damage target
  return u

distance :: Pos -> Pos -> Int
distance (ax,ay) (bx,by) = abs (ax - bx) + abs (ay - by)

doestarget :: U -> U -> Bool
u `doestarget` t = utype t /= utype u

isinrange :: U -> U -> Bool
isinrange U{upos=(ax,ay)} U{upos=(bx,by)} =
     abs (ax-bx)==1 && ay==by
  || abs (ay-by)==1 && ax==bx

isempty :: W -> (X,Y) -> Bool
isempty W{..} (x,y) = not $ any (\U{upos=(ux,uy)} -> ux==x && uy==y) wunits

adjacentspaces :: W -> U -> [(X,Y)]
adjacentspaces W{..} U{upos=(ux,uy)} =
  let (_,(xmax,ymax)) = A.bounds wmap
  in
    filter (\(x,y) -> all id [x>=0, x<=xmax, y>=0, y<=ymax]) $
    [(ux,uy-1), (ux-1,uy), (ux+1,uy), (ux,uy+1)]

-- display. these return the unmodified World for easier chaining

ux = fst . upos
uy = snd . upos

printworld :: W -> IO W
printworld w@W{..} = do
  printf "%d:  \n%s\n" wtime (ppShow wunits)
  let (_,(xmax,ymax)) = A.bounds wmap
      bgcs  = fmap showtile wmap
      allcs = bgcs A.// [ ((ux u, uy u), showunit u) | u <- wunits]
  mapM_ putStrLn [ [allcs A.! (x,y) | x <- [0..xmax]] | y <- [0..ymax] ]
  putStrLn ""
  return w

printstats w@W{..} = do
  when (wtime `mod` 1000 == 0) $ printf "%4d (%4d units)\n" wtime (length wunits)
  return w

printdots w@W{..} = do
  when (wtime `mod` 1000 == 0) (putStr ".")
  return w

