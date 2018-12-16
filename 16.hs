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

data W = W { -- simulation world
   wtime    :: Ticks
  ,wend     :: Maybe String  --  reason for ending
} deriving (Eq,Show)
type Ticks = Int -- simulation time, 0..
type Seconds = Double  -- real world time
type Delay = Seconds  -- a length of time to pause; negative means wait for input
type X = Int
type Y = Int
type P = (X,Y)

parse :: HasCallStack => String -> W
parse s =
  let
    ls = lines s
    h  = length ls
    w  = length $ head ls
  in
    W{wtime   = 0
     ,wend    = Nothing
    }

-- main

main :: HasCallStack => IO ()
main = do
  -- args <- getArgs
  -- progname <- getProgName
  -- let (usage, defargs) = ("Usage: "++progname++" [INPUTFILE]", [progname++".in"])
  -- let [f] = take 1 $ args ++ drop (length args) defargs
  -- input <- readFile f
  let input = e1

  -- part 1
  (t,w) <- timeItT $ bracket_ initterm resetterm $
    iterateUntilM (isJust.wend)
      (update >=> displayworld (-1)) <=< displayworld (-1)
      $ parse input
  printsummary w t

  -- part 2

-- update

update :: HasCallStack => W -> IO W
update w = do
  -- "Regardless of how the unit's turn ends, the next unit in the round takes its turn."
  return $
    w{ wtime = wtime w + 1
      } -- & if (wunits w'==wunits w) then endworld "units have stabilised" else id

-- display. these return the unmodified World for easier chaining

printworld :: W -> IO W
printworld w@W{..} = do
  -- printf "%d:  \n%s\n" wtime (ppShow wunits)
  -- let (_,(xmax,ymax)) = A.bounds wmap
  --     bgcs  = fmap showtile wmap
  --     allcs = bgcs A.// [ ((ux u, uy u), showunit u) | u <- wunits]
  -- mapM_ putStrLn [ [allcs A.! (x,y) | x <- [0..xmax]] | y <- [0..ymax] ]
  putStrLn ""
  return w

printstats w@W{..} = do
  when (wtime `mod` 1000 == 0) $ printf "%4d\n" wtime
  return w

printdots w@W{..} = do
  when (wtime `mod` 1000 == 0) (putStr ".")
  return w

printsummary :: W -> Double -> IO ()
printsummary W{..} t = do
  -- let (_,(_,ymax)) = A.bounds wmap
  -- setCursorPosition (ymax+3) 0
  printf "\n%s in %dth tick\n" (fromMaybe "" wend) wtime
  -- printf "%.3fs (%.0f ticks/s)\n" t wtime (fromIntegral wtime / t)

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
  --     (_,(xmax,ymax)) = A.bounds wmap
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
  -- let (_,(_,ymax)) = A.bounds wmap
  setCursorPosition (height - 20) 0
  putStrLn $ label ++ ":\n" ++ s
  pause w d

ttrace :: Show a => W -> String -> a -> a
ttrace w msg x =
  unsafePerformIO (displayinfo w 0 msg (ppShow x)) `seq`
  x

