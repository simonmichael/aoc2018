#!/usr/bin/env stack
{- stack --resolver=nightly-2018-12-12 script --compile
   --package ansi-terminal
   --package astar
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
import Data.Graph.AStar
import qualified Data.HashSet  as H
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
import Safe
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

t1 = [here|
#######
#E..G.#
#...#.#
#.G.#G#
#######
|]
-- Targets:      In range:     Reachable:    Nearest:      Chosen:
-- #######       #######       #######       #######       #######
-- #E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
-- #...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
-- #.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
-- #######       #######       #######       #######       #######


t2 = [here|
#######
#.E...#
#...?.#
#..?G?#
#######
|]
-- In range:     Nearest:      Chosen:       Distance:     Step:
-- #######       #######       #######       #######       #######
-- #.E...#       #.E...#       #.E...#       #4E212#       #..E..#
-- #...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
-- #..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
-- #######       #######       #######       #######       #######

-- Initially:
t3 = [here|
#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########
|]

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

type T = Int -- simulation ticks, 0..
data W = W { -- simulation world
   wtime    :: T
  ,wmap     :: A.Array Pos Tile
  ,wunits   :: [U]
  ,wend     :: Maybe String
} deriving (Eq,Show)
data Tile = Wall | Floor deriving (Eq,Show)
data U = U {
   utype    :: Type
  ,uhp      :: HP
  ,upos     :: Pos
} deriving (Eq,Show)
data Type = E | G deriving (Eq,Show)  -- elf, goblin
type HP = Int
type X = Int
type Y = Int
type Pos = (X,Y)
type Path = [Pos]
type Delay = Double

parse :: HasCallStack => String -> W
parse s =
  let
    ls = lines s
    h  = length ls
    w  = length $ head ls
  in
    W{wtime   = 0
     ,wmap    = A.array ((0,0),(w-1,h-1)) [ ((x,y), parsetile c) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l ]
     ,wunits  = catMaybes $ concat [ [(parseunit x y c) | (x,c) <- zip [0..] l ] | (y,l) <- zip [0..] ls ]
     ,wend    = Nothing
    }

parsetile '#' = Wall
parsetile _   = Floor

showtile Wall  = '#'
showtile Floor = '.'

parseunit x y 'E' = Just $ U {utype=E, upos=(x,y), uhp=defhp}
parseunit x y 'G' = Just $ U {utype=G, upos=(x,y), uhp=defhp}
parseunit _ _ _   = Nothing

showunit :: HasCallStack => U -> Char
showunit U{..} = head $ show utype

defhp = 200

-- main

main :: HasCallStack => IO ()
main = do
  let (usage,defargs) = ("Usage: ./15 [INPUTFILE]", ["15.in"])
  args <- getArgs
  -- -- when (null args) $ putStrLn usage >> exitSuccess
  let [f] = take 1 $ args ++ drop (length args) defargs
  input <- readFile f

  -- part 1
  -- (t,w) <- timeItT $ iterateUntilM ((==2).wtime) (update >=> printworld) <=< printworld $ parse t1
  -- printfinaltime w t

  (t,w) <- timeItT $
     bracket_ initterm resetterm $
       iterateUntilM (isJust.wend) (update >=> displayworld (0)) <=< displayworld (0) $ parse input
  printfinalsummary w t

  -- part 2
  -- (t,w) <- timeItT $ iterateUntilM ((==1000).wtime) (update2 >=> printworld) $ parse t1
  -- printf "%.3fs to simulate %d ticks (%.0f ticks/s)\n" t (wtime w) (fromIntegral (wtime w) / t)
  -- let x = wx w
  -- putStr x >> assert_ (x == 123) >> putStrLn ": ok"

-- update

update :: HasCallStack => W -> IO W
update w = do
  -- "Regardless of how the unit's turn ends, the next unit in the round takes its turn."
  w' <- foldM unitupdate w (sortunits $ wunits w)
  return $
    w'{ wtime = wtime w + 1
      } & if (wunits w'==wunits w) then endworld "units have stabilised" else id

-- replace a unit in the units list.
-- The new/updated unit will be inserted at the front of the list.
worldreplaceunit :: W -> U -> U -> W
worldreplaceunit w@W{..} oldu newu = w{ wunits = newu : (wunits \\ [oldu]) }

-- trigger the end of the world, and set the reason unless it's already set
endworld :: String -> W -> W
endworld reason w@W{..} = w{wend=maybe (Just reason) Just wend}

sortunits :: [U] -> [U]
sortunits = sortOn (\U{upos=(x,y)} -> (y,x))

sortpoints :: [Pos] -> [Pos]
sortpoints = sortOn (\(x,y) -> (y,x))

displayhighlightunits w d us = do
  forM_ us $ \u -> do
    displaypoints w d (showunit u) [upos u] [SetSwapForegroundBackground True]
    setSGR [Reset]

-- perform this unit's turn, updating it and possibly other units in the world
unitupdate :: HasCallStack => W -> U -> IO W
unitupdate w u = do
  unitmove w u >>= flip unitattack u

unitmove :: HasCallStack => W -> U -> IO W
unitmove w@W{..} u = do
  let targets = filter (u `doestarget`) wunits
      inrange = filter (isinrange u) targets
  displayhighlightunits w 0.1 [u]
  -- displayworld 0 w >> displaypoints w 0 'T' (map upos targets) [] >> displayinfo w "targets" (ppShow $ map upos targets)
  -- displayworld 0 w >> displaypoints w 0 'I' (map upos inrange) [] >> displayinfo w "in range" (ppShow $ map upos inrange)
  --  if not in range, find shortest path to a reachable in-range space
  mpath <- case inrange of
          _:_ -> displayworld 0 w >> return Nothing
          []  ->
            let
              dests         = concatMap (emptyadjacentspaces w . upos) targets
              shortestpaths = catMaybes $ map (astarshortestpathreadorder w (upos u)) dests
            in
              case shortestpaths of
                []    -> return Nothing
                paths -> do
                  let reachable             = nub $ sortpoints $ map last paths
                      shortestshortestpaths = head $ groupBy ((==)`on`length) $ sortOn length paths
                      nearestdests          = nub $ sort $ map last shortestshortestpaths
                      dest                  = head $ sortpoints nearestdests
                      destpaths             = filter ((==dest).last) shortestshortestpaths
                      chosenpath            = head $ sortOn (\((x,y):_) -> (y,x)) $ destpaths
                  -- displayworld 0 w >> displaypoints w 0 '?' dests        [] >> displayinfo w "dests" (ppShow dests) >> doinput w
                  -- displayworld 0 w >> displaypoints w 0 '@' reachable    [] >> displayinfo w "reachable" (ppShow reachable) >> doinput w
                  -- displayinfo w "all dests' shortest paths" (ppShow shortestpaths) >> doinput w
                  -- displayworld 0 w >> displayinfo w "shortestshortestpaths" (ppShow shortestshortestpaths) >> doinput w
                  -- displayworld 0 w >> displaypoints w 0 '!' nearestdests [] >> displayinfo w "nearestdests" (ppShow nearestdests) >> doinput w
                  -- displayworld 0 w >> displayinfo w "dest" (show dest) >> doinput w
                  -- displayworld 0 w >> displayinfo w "destpaths" (ppShow destpaths) >> doinput w
                  displayworld 0 w
                    >> displayhighlightunits w 0 [u]
                    >> displaypoints w 0 '.' chosenpath []
                    >> displaypoints w 0.3 '+' [dest] []
                    -- >> doinput w
                  displayworld 0 w
                  return $ Just chosenpath

  return $ case mpath of
             Just (nextpos:_) -> worldreplaceunit w u u{upos=nextpos}
             Nothing          -> w

unitattack :: HasCallStack => W -> U -> IO W
unitattack w@W{..} u = do
  -- if target-adjacent
  --  select lowest-hp adjacent target
  --  damage target
  return w

displayinfo W{..} label s = do
  let (_,(_,ymax)) = A.bounds wmap
  setCursorPosition (ymax+3) 0
  putStrLn $ label ++ ":\n" ++ s

ttrace :: Show a => W -> String -> a -> a
ttrace w msg x =
  unsafePerformIO (displayinfo w msg (ppShow x)) `seq`
  x

-- the astar lib returns only one shortest path, so run it from each
-- adjacent position and if there are several with the shortest length
-- pick the reading-order one
astarshortestpathreadorder :: HasCallStack => W -> Pos -> Pos -> Maybe Path
astarshortestpathreadorder w@W{..} startpos endpos =
  let
    starts'       = emptyadjacentspaces w startpos
    paths         = [(s, fromJust p) | s <- starts', let p = astarshortestpath w s endpos, isJust p]
    shortestpaths = headMay $ groupBy ((==) `on` (length.snd)) $ sortOn (length.snd) paths
  in
    case shortestpaths of
      Nothing      -> Nothing
      Just [(s,p)] -> Just $ s:p
      Just ps      -> Just $ s:p where (s,p) = head $ sortOn (\(_,((x,y):_)) -> (y,x)) ps

astarshortestpath :: W -> Pos -> Pos -> Maybe Path
astarshortestpath w@W{..} startpos endpos =
  aStar
    (H.fromList . emptyadjacentspaces w) -- (a -> HashSet a) The graph we are searching through, given as a function from vertices to their neighbours.
    distance                             -- (a -> a -> c)	   Distance function between neighbouring vertices of the graph. This will never be applied to vertices that are not neighbours, so may be undefined on pairs that are not neighbours in the graph.
    (distance startpos)                  -- (a -> c)	       Heuristic distance to the (nearest) goal. This should never overestimate the distance, or else the path found may not be minimal.
    (==endpos)                           -- (a -> Bool)	     The goal, specified as a boolean predicate on vertices.
    startpos                             -- a	               The vertex to start searching from.

distance :: Pos -> Pos -> Int
distance (ax,ay) (bx,by) = abs (ax - bx) + abs (ay - by)

doestarget :: U -> U -> Bool
u `doestarget` t = utype t /= utype u

isinrange :: U -> U -> Bool
isinrange U{upos=(ax,ay)} U{upos=(bx,by)} =
     abs (ax-bx)==1 && ay==by
  || abs (ay-by)==1 && ax==bx

adjacentspaces :: W -> Pos -> [Pos]
adjacentspaces W{..} (ux,uy) =
  let (_,(xmax,ymax)) = A.bounds wmap
  in
    filter (\(x,y) -> all id [x>=0, x<=xmax, y>=0, y<=ymax]) $
    [(ux,uy-1), (ux-1,uy), (ux+1,uy), (ux,uy+1)]

emptyadjacentspaces :: W -> Pos -> [Pos]
emptyadjacentspaces w = filter (isempty w) . adjacentspaces w

isempty :: W -> (X,Y) -> Bool
isempty W{..} (x,y) =
     wmap A.! (x,y) == Floor
  && not (any (\U{upos=(ux,uy)} -> ux==x && uy==y) wunits)

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

printfinalsummary :: W -> Double -> IO ()
printfinalsummary W{..} t = do
  printf "\n%s\n" (fromMaybe "" wend)
  printf "%.3fs to simulate %d ticks (%.0f ticks/s)\n" t wtime (fromIntegral wtime / t)
  -- doinput w

initterm = do
  hideCursor
  hSetEcho stdout False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

resetterm = do
  setSGR [Reset]
  showCursor

toscreenx = (+1)
toscreeny = (+2)

-- display a character with a style at some positions, and pause
displaypoints :: W -> Delay -> Char -> [Pos] -> [SGR] -> IO W
displaypoints w d c ps style = do
  setSGR style
  forM_ ps $ \(x,y) -> do
    setCursorPosition (toscreeny y) (toscreenx x)
    putChar c
  delay d
  return w

-- display in an ansi terminal and pause for the given number of seconds
-- (or if negative, wait and handle keypress)
displayworld :: HasCallStack => Delay -> W -> IO W
displayworld delaysecs w@W{..} = do
  Just (Window{..}) <- size

  setSGR [
     SetColor Background Dull Black
    ,SetColor Foreground Vivid White
    ,SetConsoleIntensity BoldIntensity
    ,SetSwapForegroundBackground False
    ]
  setCursorPosition 0 0
  clearScreen
  putStrLn $ "t " ++ show wtime ++ "  "

  setSGR [
     SetColor Foreground Dull Red
    ,SetColor Background Dull Black
    ,SetConsoleIntensity FaintIntensity
    ,SetSwapForegroundBackground False
    ]
  let bg = fmap showtile wmap
      (_,(xmax,ymax)) = A.bounds wmap
  putStrLn $ " " ++ concatMap (take 1.reverse.show) [0..xmax]
  mapM_ putStrLn [ (take 1.reverse.show) y ++ [bg A.! (x,y) | x <- [0..xmax]] | y <- [0..ymax] ]

  setSGR [
     SetColor Background Dull Black
    ,SetColor Foreground Vivid White
    ,SetConsoleIntensity BoldIntensity
    ,SetSwapForegroundBackground False
    ]
  forM_ wunits $ \u@U{upos=(x,y),..} -> do
    setCursorPosition (toscreeny y) (toscreenx x)
    putChar $ showunit u

  -- position cursor for debug printing
  setCursorPosition (ymax+3) 0

  if delaysecs >=0
  then delay delaysecs
  else void $ doinput w
  return w

delay secs = threadDelay $ round $ secs * 1e6

doinput w@W{..} = do
  displayprompt
  c <- getChar
  case c of
    'q' -> exitSuccess
    'i' -> displayinfo w "units" (ppShow wunits) >> doinput w
    _   -> return w
  
displayprompt = do
  Just (Window{..}) <- size
  setSGR [
     SetColor Foreground Dull White
    ,SetColor Background Dull Black
    ,SetConsoleIntensity FaintIntensity
    ,SetSwapForegroundBackground False
    ]
  setCursorPosition (height-4) 0
  putStrLn $ "\n\nq: quit,  i: info,  any other key: advance"

