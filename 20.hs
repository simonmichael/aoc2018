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
   --package astar
   --package relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Array (Array,array,assocs,listArray,bounds,(!),(//))
import Data.Bifunctor
import Data.CallStack
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Graph
-- import Data.Graph.AStar
import qualified Data.HashSet  as H
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
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
import Data.Tuple
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

e1 = [here|
|]

e2 = [here|
|]

e3 = [here|
|]

e4 = [here|
|]

e5 = [here|
|]

e6 = [here|
|]

-- data

type Ticks = Int -- simulation time, 0..
type Seconds = Double  -- real world time
type Delay = Seconds  -- a length of time to pause; negative means wait for input
type X = Int
type Y = Int
type P = (X,Y)

data W = W { -- simulation world
   wtime    :: Ticks
  ,wend     :: Maybe String
  ,wdirections    :: Directions
  ,wrooms         :: [Room]
  ,wbounds        :: ((X,Y),(X,Y))
  ,wroompositions :: M.Map Room P
  ,wpositionrooms :: M.Map P Room
  ,wroomg         :: Graph
} deriving (Eq,Show)

-- ^EENWES$
-- ^ENWWW(NEEE|SSE(EE|N))$
type Directions = String

type Room = Int

type Path  = [P]  -- a path through point space, usually between significant points
type Trail = [P]  -- a partial path, going nowhere special

-- | A branch point, and (for most recent branch point only)
-- any branch trails originating from it which have been parsed
-- but not yet handled.
type BranchInfo = (P,[Trail])

move 'N' (x,y) = (x,y-1)
move 'S' (x,y) = (x,y+1)
move 'E' (x,y) = (x+1,y)
move 'W' (x,y) = (x-1,y)

-- | Extend this trail one step at the end in the given compass direction.
-- An empty trail is extended from 0,0.
extend :: Trail -> Char -> Trail
extend [] d = [move d (0,0)]
extend ps d = ps ++ [move d $ last ps]

-- | Like extend, but only if a direction is provided.
extend' :: Trail -> Maybe Char -> Trail
extend' t Nothing  = t
extend' t (Just c) = extend t c

-- | Make a simple trail by following the given directions, containing no branches,
-- from the starting point.
maketrail :: P -> Directions -> Trail
maketrail p ds = foldl' extend [p] ds
            
-- | Generate all possible trail fragments from the given directions,
-- which may contain branches, starting from 0,0.
-- The union of these trails' edges can be used to build a complete graph.
trails :: Directions -> [Trail]
trails ds  = trails' (0,0) [] ds

-- | trails helper: follow the directions from the given point, exploring
-- every branch of alternatives, generating all possible trail fragments.
-- The end points of these trails will overlap so that the union of 
-- their edges can be used to build a complete graph.
--
-- Keeps a stack of info regarding branches currently being parsed, most recent first.
-- A '(' causes the most recent point to be pushed onto the stack as a branch point.
-- On reaching '|', the trail just parsed since most recent branch point is also stored
-- along with that branch point
-- On reaching ')', the most recent branch info is popped, and any stored 
-- branch trails, plus the final branch trail, are each extended by all remaining
-- directions/alternatives. (Ie every '(' splits the world of trails,
-- potentially lots of duplication/inefficiency.)
--
-- >>> mapM_ print $ trails' (0,0) [] "E(N|S)E"
-- [(0,0),(1,0)]
-- [(1,0),(1,-1)]
-- [(1,0),(1,1)]
-- [(1,-1),(2,-1)]
-- [(1,1),(2,1)]
-- 
trails' :: P -> [BranchInfo] -> Directions -> [Trail]
trails' _ _ "" = []
trails' tstart branches ds =
  -- trace ("trails': "++show (tstart,ds,branches)) () `seq`
  let
    (dsnext,drest)   = break (`elem` "(|)") ds
    (d:drest')       = drest
    (bpoint,btrails) = headDef (tstart,[]) branches
    brest            = drop 1 branches
    t                = maketrail bpoint dsnext
    bts              = btrails ++ [t]
    tend             = lastDef tstart t
  in
    if | null drest -> [t]
       | d=='('     -> [t] ++ trails' tend ((tend,[]):branches) drest'
       | d=='|'     -> trails' tstart ((bpoint, btrails++[t]):brest) drest'
       | d==')'     -> bts ++ concat [trails' (last bt) (drop 1 branches) drest' | bt <- bts]
-- XXX
-- *Main> pp $ trails "E(N|E)E"
-- [ [ ( 0 , 0 ) , ( 1 , 0 ) ]
-- , [ ( 1 , 0 ) , ( 1 , -1 ) ]
-- , [ ( 1 , 0 ) , ( 2 , 0 ) ]
-- , [ ( 1 , -1 ) , ( 2 , -1 ) ]
-- , [ ( 2 , 0 ) , ( 3 , 0 ) ]
-- ]
-- *Main> pp $ trails "(E(N|E)E)"
-- [ [ ( 0 , 0 ) ]
-- , [ ( 0 , 0 ) , ( 1 , 0 ) ]
-- , [ ( 1 , 0 ) , ( 1 , -1 ) ]
-- , [ ( 1 , 0 ) , ( 2 , 0 ) ]
-- , [ ( 0 , 0 ) , ( 1 , 0 ) ]
-- , [ ( 0 , 0 ) , ( 1 , 0 ) ]
-- ]

-- | From a directions string, build
-- - a list of unique rooms in order visited
-- - minimum and maximum x,y positions
-- - a map from room to position
-- - a map from position to room
-- - a graph of room connections
build :: Directions -> ([Room], ((X,Y),(X,Y)), M.Map Room P, M.Map P Room, Graph)
build ds =
  let
    -- trails generated, in sequence
    ts =
      -- ltrace "ts" $
      trails ds
    -- positions visited, in sequence, excluding duplicates
    ps =
      -- ltrace "psvisited" $
      nub $ concat ts
    -- position bounds
    (xs,ys) = (map fst ps, map snd ps)
    bounds = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    -- assign room numbers to positions in order visited
    positionrooms =
      -- ltrace "positionrooms" $
      M.fromList $ reverse $ zip ps [0..]
    -- map from room numbers to positions
    roompositions = M.fromList $ map swap $ M.assocs positionrooms
    -- unique room numbers in order visited (maybe with branches interleaved ?)
    toroom = fromJust . flip M.lookup positionrooms
    rooms =
      -- ltrace "rooms" $
      map toroom ps
    -- room connections, both ways
    roomedges =
      -- ltrace "roomedges" $
      nub $ sort $ concat [
        concat [[(a,b), (b,a)] | (a,b) <- zip rs (drop 1 rs)]
        | ps <- ts, let rs = map toroom ps
        ]
    -- graph of room connections
    roomg =
      -- ltrace "roomg" $
      buildG (minimum rooms, maximum rooms) roomedges
  in
    (rooms, bounds, roompositions, positionrooms, roomg)

parse :: HasCallStack => String -> W
parse s =
    W{wtime   = 0
     ,wend    = Nothing
     ,wdirections    = s
     ,wrooms         = rooms
     ,wbounds        = bounds
     ,wroompositions = roompositions
     ,wpositionrooms = positionrooms
     ,wroomg         = roomg
     }
  where
    (rooms, bounds, roompositions, positionrooms, roomg) = build s

parsedirections :: HasCallStack => String -> Directions
parsedirections = drop 1 .init

gbounds g = (minimum vs, maximum vs) where vs = vertices g

gdel :: Vertex -> Graph -> Graph
gdel v g =
  let
    vs  = vertices g \\ [v]  -- v may get recreated, with no edges
    b   = (minimum vs, maximum vs)
    es' = [ (a,b) | (a,b) <- edges g, not $ v `elem` [a,b] ]
  in
    buildG b es'
    
edgesfrom g v = [(a,b) | (a,b) <- edges g, a==v]

-- Longest path between start and end vertices in a graph.
longestPath :: Graph -> Vertex -> Vertex -> [Edge]
longestPath g start end
  | start == end = []
  | otherwise    = 
    maximumBy (comparing length) $ ([]:)
              [ e : path | e@(_,start') <- edgesfrom g start
                         , let g' = gdel start g
                         , let path = longestPath g' start' end ]

-- Number of steps to the farthest room from room 0, and it's number and position.
farthestRoom :: W -> (Int, Room, P)
farthestRoom w@W{..} =
  let
    start = 0
    ends = vertices wroomg \\ [start]
    longestpath =
      maximumBy (comparing length) [
        traceWith ((("longest to "++show end++": ")++).show.length) $
        longestPath wroomg start end | end <- ends ]
    room = snd $ last longestpath
  in
    (length longestpath, room, fromJust $ M.lookup room wroompositions)

-- main

main :: HasCallStack => IO ()
main = do
  -- part 1

  -- To get a sense for the size of this facility, you'd like to determine
  -- which room is furthest from you: specifically, you would like to find
  -- the room for which the shortest path to that room would require
  -- passing through the most doors.
  
  -- What is the largest number of doors you would be required
  -- to pass through to reach a room? That is, find the room for which
  -- the shortest path from your starting location to that room would
  -- require passing through the most doors; what is the fewest doors
  -- you can pass through to reach it?
  
  -- print $ farthestRoom $ parse "WNE"
  -- print $ farthestRoom $ parse "ENWWW(NEEE|SSE(EE|N))"
  -- print $ farthestRoom $ parse "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN"
  -- print $ farthestRoom $ parse "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))"
  -- print $ farthestRoom $ parse "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"
  ds <- parsedirections . init <$> readFile "20.in"
  timeIt $ print $ farthestRoom $ parse $ take 150 ds
  -- timeIt $ print $ farthestRoom $ parse $ take 1000 ds
  -- timeIt $ print $ farthestRoom $ parse $ take 2000 ds
  -- timeIt $ print $ farthestRoom $ parse $ take 3000 ds
  -- timeIt $ print $ farthestRoom $ parse $ take 4000 ds
  -- timeIt $ print $ farthestRoom $ parse ds

  -- pp w
  -- printworld w
  return ()

  -- (t,w) <- timeItT $ -- bracket_ initterm resetterm $
  --   iterateUntilM (isJust.wend)
  --     (update >=> printworld) <=< printworld
  --     -- (update >=> displayworld (-1)) <=< displayworld (-1)
  --     $ parse e1
  -- printsummary w t

-- part 2

-- update

-- -- trigger the end of the world, and set the reason unless it's already set
-- worldend :: String -> W -> W
-- worldend reason w@W{..} = w{wend=maybe (Just reason) Just wend}

-- update :: HasCallStack => W -> IO W
-- update w = do
--   let w' = w
--   let w'' =
--         w'{
--          wtime = wtime w + 1
--         } & if (w'' == w) then worldend "world has stabilised" else id
--   return w''

-- display. these return the unmodified World for easier chaining

-- convert map coords to screen coords leaving space for heading, axes etc.
toscreenx = (+5)
toscreeny = (+5)

printworld :: W -> IO W
printworld w@W{..} = do
  printf "\n%d:  \n" wtime
  printrooms w

printrooms w@W{..} = do
  let ((xmin,ymin),(xmax,ymax)) = wbounds
  let row y = '\n' : concat [maybe "   " (printf "%3d") $
                              M.lookup (x,y) wpositionrooms | x <- [xmin..xmax]]
  let rows = [row y | y <- [ymin..ymax]]
  mapM_ putStrLn rows
  return w

printsummary :: W -> Double -> IO ()
printsummary W{..} t = do
  -- let (_,(_,ymax)) = bounds wmap
  -- setCursorPosition (ymax+3) 0
  -- printf "\n%s in %dth tick\n" (fromMaybe "" wend) wtime
  printf "%.3fs (%.0f ticks/s)\n" t wtime (fromIntegral wtime / t)

{-

printstats w@W{..} = do
  when (wtime `mod` 1000 == 0) $ printf "%4d\n" wtime
  return w

printdots w@W{..} = do
  when (wtime `mod` 1000 == 0) (putStr ".")
  return w

initterm = do
  hideCursor
  hSetEcho stdout False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

resetterm = do
  setSGR [Reset]
  showCursor

-- display in an ansi terminal and pause for the given number of seconds
-- (or if negative, wait and handle keypress)
displayworld :: HasCallStack => Delay -> W -> IO W
displayworld d w@W{..} = do
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
     SetColor Background Dull Black
    ,SetColor Foreground Dull Red
    ,SetConsoleIntensity FaintIntensity
    ,SetSwapForegroundBackground False
    ]
  -- let bg = fmap showtile wmap
  --     (_,(xmax,ymax)) = bounds wmap
  -- putStrLn $ " " ++ concatMap (take 1.reverse.show) [0..xmax]
  -- let maprows   = [ lastdigit y : [bg A.! (x,y) | x <- [0..xmax]]
  --                   | y <- [0..ymax] ]
  --     unitstats = [ intercalate ", " $ map showunithp $ sortOn ux $ filter ((==y).uy) wunits
  --                   | y <- [0..ymax] ]
  -- forM_ (take (max (length maprows) (length unitstats)) $ zip
  --         (maprows++repeat (replicate (xmax+1) ' '))
  --         (unitstats++repeat "")) $
  --   \(mr,us) -> putStrLn $ mr ++ "    " ++ us

  setSGR [
     SetColor Background Dull Black
    ,SetColor Foreground Vivid White
    ,SetConsoleIntensity BoldIntensity
    ,SetSwapForegroundBackground False
    ]
  -- forM_ wunits $ \u@U{upos=(x,y),..} -> do
  --   setCursorPosition (toscreeny y) (toscreenx x)
  --   putChar $ showunit u

  -- position cursor for debug printing
  setCursorPosition (height-20) 0

  pause w d
  return w

-- wait for n seconds, or if n is negative, prompt for and handle a keypress
pause :: W -> Delay -> IO ()
pause w d = if d >=0 then delay d else void $ doinput w

-- wait for n seconds
delay :: Seconds -> IO ()
delay secs = threadDelay $ round $ secs * 1e6

-- prompt for and handle a keypress. Does not change the world.
doinput :: W -> IO ()
doinput w@W{..} = do
  displayprompt
  c <- getChar
  case c of
    'q' -> exitSuccess
    -- 'i' -> displayinfo w (-1) "units" (ppShow wunits)
    _   -> return ()
  
displayprompt = do
  Just (Window{..}) <- size
  setSGR [
     SetColor Background Dull Black
    ,SetColor Foreground Dull White
    ,SetConsoleIntensity FaintIntensity
    ,SetSwapForegroundBackground False
    ]
  setCursorPosition (height-4) 0
  putStrLn $ "\n\nq: quit,  any other key: advance"

-- display a character with a style at some positions, and pause
displaypoints :: W -> Delay -> Char -> [P] -> [SGR] -> IO W
displaypoints w d c ps style = do
  setSGR style
  forM_ ps $ \(x,y) -> do
    setCursorPosition (toscreeny y) (toscreenx x)
    putChar c
  pause w d
  return w

displayinfo :: W -> Delay -> String -> String -> IO ()
displayinfo w@W{..} d label s = do
  Just (Window{height,width}) <- size
  -- let (_,(_,ymax)) = bounds wmap
  setCursorPosition (height - 20) 0
  putStrLn $ label ++ ":\n" ++ s
  pause w d

ttrace :: Show a => W -> String -> a -> a
ttrace w msg x =
  unsafePerformIO (displayinfo w 0 msg (ppShow x)) `seq`
  x

-}
