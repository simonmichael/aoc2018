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
} deriving (Eq,Show)

type Directions = String

parsedirections :: HasCallStack => String -> Directions
parsedirections = drop 1 .init

type Room = Int

-- from a string of directions, build
-- - a list of unique rooms in order visited
-- - a map from room to position
-- - a map from position to room
-- - a graph of room connections
build :: String -> ([Room], M.Map Room P, M.Map P Room, Graph)
build directions =
  let
    -- positions in order visited
    psvisited = reverse $ foldl' follow [(0,0)] directions
    -- assign room numbers to positions in order visited
    roomsbyp = M.fromList $ reverse $ zip psvisited [0..]
    -- map from room numbers to positions
    roompositions = M.fromList $ map swap $ M.assocs roomsbyp
    -- room numbers in order visited
    roomvisits = map (fromJust . flip M.lookup roomsbyp) psvisited
    -- room connections, both ways
    roomedges = concat [[(a,b), (b,a)] | (a,b) <- zip roomvisits (drop 1 roomvisits)]
    -- unique room numbers in order visited
    rooms = nub roomvisits
    -- graph of rooms
    roomg = buildG (minimum rooms, maximum rooms) roomedges
  in
    (rooms, roompositions, roomsbyp, roomg)

follow :: [P] -> Char -> [P]
follow ps@((x,y):_) d = go d:ps
  where
    go 'N' = (x,y-1)
    go 'S' = (x,y+1)
    go 'E' = (x+1,y)
    go 'W' = (x-1,y)


parse :: HasCallStack => String -> W
parse s =
    W{wtime   = 0
     ,wend    = Nothing
     -- ,wdirections = drop 1 $ init s
    }

-- main

main :: HasCallStack => IO ()
main = do
  -- part 1
-- What is the largest number of doors you would be required
-- to pass through to reach a room? That is, find the room for which
-- the shortest path from your starting location to that room would
-- require passing through the most doors; what is the fewest doors
-- you can pass through to reach it?
  (t,w) <- timeItT $ -- bracket_ initterm resetterm $
    iterateUntilM (isJust.wend)
      (update >=> printworld) <=< printworld
      -- (update >=> displayworld (-1)) <=< displayworld (-1)
      $ parse e1
  printsummary w t

-- part 2

-- update

-- trigger the end of the world, and set the reason unless it's already set
worldend :: String -> W -> W
worldend reason w@W{..} = w{wend=maybe (Just reason) Just wend}

update :: HasCallStack => W -> IO W
update w = do
  let w' = w
  let w'' =
        w'{
         wtime = wtime w + 1
        } & if (w'' == w) then worldend "world has stabilised" else id
  return w''

-- display. these return the unmodified World for easier chaining

-- convert map coords to screen coords leaving space for heading, axes etc.
toscreenx = (+5)
toscreeny = (+5)

printworld :: W -> IO W
printworld w@W{..} = do
  printf "\n%d:  \n" wtime
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
