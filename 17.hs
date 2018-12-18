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
-- import Data.Graph.AStar
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
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
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

-- plan:
-- water will be stored as tiles in the map
-- on each turn, every water tile will be moved, lowest & outermost first, leaving wet sand behind
-- with sand underneath, it will fall
-- or with sand to the left or right, preferring the outermost direction, it will move there

type Ticks = Int -- simulation time, 0..
type Seconds = Double  -- real world time
type Delay = Seconds  -- a length of time to pause; negative means wait for input
type X = Int
type Y = Int
type P = (X,Y)
data W = W { -- simulation world
   wtime    :: Ticks
  ,wend     :: Maybe String  --  reason for ending
  ,wmap     :: A.Array P Tile
} deriving (Eq,Show)
data Tile = Clay | DrySand | WetSand | Water | Spring deriving (Eq,Show)

parsetile '#' = Clay
parsetile '.' = DrySand -- sand where water has never been
parsetile '|' = WetSand -- sand where water has passed through
parsetile '~' = Water   -- water-logged sand
parsetile '+' = Spring  -- water source

showtile Clay    = '#'
showtile DrySand = '.'
showtile WetSand = '|'
showtile Water   = '~'
showtile Spring  = '+'

-- the spring is always at 500,0
(springx,springy) = (500,0)

-- parse clay positions, normalising x coordinates with x=0 at the spring,
-- and generate the initial map.
-- x=495, y=2..7
-- y=7, x=495..501
-- the .. is always on the right
parse :: HasCallStack => String -> W
parse s =
  let
    keepdigit c = if isDigit c then c else ' '
    claypsfrom l
      | head l == 'y' =
          [(x-springx,y) | let [y,x1,x2] = map read $ words $ map keepdigit l, x <- [x1..x2]]
      | head l == 'x' =
          [(x-springx,y) | let [x,y1,y2] = map read $ words $ map keepdigit l, y <- [y1..y2]]
    clayps = concatMap claypsfrom $ lines s
    -- build an empty map with the right bounds. include the normalised spring position.
    (xs,ys) = (0 : map fst clayps, springy : map snd clayps)
    (xmin,xmax,ymin,ymax) = (minimum xs - 1, maximum xs + 1, minimum ys, maximum ys)
    m = A.listArray ((xmin,ymin),(xmax,ymax)) $ repeat DrySand
    -- add clay and the spring
    m' = m // (zip clayps (repeat Clay)) // [((0,springy),Spring)]
  in
    W{wtime    = 0
     ,wend     = Nothing
     ,wmap     = m'
    }

-- main

main :: HasCallStack => IO ()
main = do
  -- part 1
  -- How many tiles can be reached by the water? To prevent counting
  -- forever, ignore tiles with a y coordinate smaller than the smallest
  -- y coordinate in your scan data or larger than the largest one. Any
  -- x coordinate is valid. In this example, the lowest y coordinate
  -- given is 1, and the highest is 13, causing the water spring (in row
  -- 0) and the water falling off the bottom of the render (in rows 14
  -- through infinity) to be ignored.

  -- So, in the example above, counting both water at rest (~) and other
  -- sand tiles the water can hypothetically reach (|), the total number
  -- of tiles the water can reach is 57.

  -- How many tiles can the water reach within the range of y values in
  -- your scan?

  (t,w) <- timeItT $ -- bracket_ initterm resetterm $
    iterateUntilM (isJust.wend)
      (update >=> printworld) <=< printworld
      -- (update >=> displayworld (-1)) <=< displayworld (-1)
      $ parse e1
  printsummary w t

-- part 2

-- update

update :: HasCallStack => W -> IO W
update w = do
  w' <- movewater w $ waterps w
  putStrLn ""
  w'' <- makewater w'
  return $
    w''{ wtime = wtime w + 1
       } & if (wmap w'' == wmap w) then worldend "world has stabilised" else id

-- trigger the end of the world, and set the reason unless it's already set
worldend :: String -> W -> W
worldend reason w@W{..} = w{wend=maybe (Just reason) Just wend}

-- points in the map containing water
waterps :: W -> [P]
waterps W{..} = map fst $ filter ((==Water).snd) $ assocs wmap

-- the spring continually emits water
makewater w@W{..} = return w{ wmap = wmap // [((0,springy+1),Water)] }

-- to mimic natural flow, water points are always updated in a specific order:
-- 1. lowest first, and..
sortwaterps :: W -> [P] -> [P]
sortwaterps w ps =
  concatMap (sortwaterpsrow w) $
  groupBy ((==) `on` snd) $ reverse $ sortOn snd ps
    
-- sort water points on a single row, in this order:
-- 2. points which are open below, left-most first
-- 3. points which are open on the left or right, left-most first
-- 4. points which are not open below or to the side, left-most first.
sortwaterpsrow :: W -> [P] -> [P]
sortwaterpsrow w ps =
  sortOn fst bopen ++ sortOn fst sopen ++ sortOn fst nopen
  where
    (bopen,ps')   = partition (openbelow w) ps
    (sopen,nopen) = partition (openeitherside w) ps'
    
openbelow w (x,y) = isempty w (x,y+1)
openleft  w (x,y) = isempty w (x-1,y)
openright w (x,y) = isempty w (x+1,y)
openeitherside w p = openleft w p || openright w p

isempty :: W -> P -> Bool
isempty W{..} p = (wmap ! p) `elem` [DrySand,WetSand]

-- move the given water points, always starting with the lowest
-- movable point (see sortwaterps).
movewater :: HasCallStack => W -> [P] -> IO W
movewater w [] = return w
movewater w@W{..} ps = do
  let p:rest = sortwaterps w ps
  w' <- movewaterpoint w p
  movewater w' rest

-- A water point moves into wet or dry sand below it, 
-- or to left or right of it, and leaves wet sand behind.
-- when both left and right are open, it moves away from center, or if at center, to the right.
movewaterpoint :: HasCallStack => W -> P -> IO W
movewaterpoint w@W{..} p@(x,y) = do
  if | changes==movedown  -> printf "%d,%dv " x y
     | changes==moveleft  -> printf "<%d,%d " x y
     | changes==moveright -> printf "%d,%d> " x y
     | otherwise          -> printf "%d,%d. " x y
  return w{wmap = wmap // changes}
  where
    ((xmin,ymin),(xmax,ymax)) = bounds wmap
    atbottom  = y==ymax
    atleft    = x==xmin
    atright   = x==xmax
    wetsand   = [((x,y), WetSand)]
    movedown  = wetsand ++ [((x,y+1), Water)]
    moveleft  = wetsand ++ [((x-1,y), Water)]
    moveright = wetsand ++ [((x+1,y), Water)]
    changes
      | openbelow w p                        = movedown
      | openleft w p && openright w p && x<0 = moveleft
      | openleft w p && openright w p        = moveright
      | openleft w p                         = moveleft
      | openright w p                        = moveright
      | atbottom                             = wetsand
      | otherwise                            = []
          -- error $ show (x,y,xmin,xmax,ymin,ymax,
          --                 atbottom,atleft,atright,
          --                 openbelow,openleft,openright)

-- display. these return the unmodified World for easier chaining

-- convert map coords to screen coords leaving space for heading, axes etc.
toscreenx = (+5)
toscreeny = (+5)

printworld :: W -> IO W
printworld w@W{..} = do
  printf "\n%d:  \n" wtime
  let ((xmin,ymin),(xmax,ymax)) = bounds wmap
      -- xstrs   = [show $ x + springx | x <- [xmin..xmax]]
      -- xstrs   = [s ++ show x | x <- [xmin..xmax], let s = if x<0 then "" else " "]
      xstrs   = [show $ abs x | x <- [xmin..xmax]]
      longest = maximum $ map length xstrs
      padded  = map (take longest.(++repeat ' ')) xstrs
      leftpad = replicate (toscreenx 0) ' '
  mapM_ (putStrLn . (leftpad++)) $ transpose padded
  let chararray = fmap showtile wmap
  mapM_ putStrLn [ printf "%4d " y ++ [chararray ! (x,y) | x <- [xmin..xmax]]
                 | y <- [ymin..ymax] ]
  return w

printsummary :: W -> Double -> IO ()
printsummary W{..} t = do
  -- let (_,(_,ymax)) = bounds wmap
  -- setCursorPosition (ymax+3) 0
  printf "\n%s in %dth tick\n" (fromMaybe "" wend) wtime
  -- printf "%.3fs (%.0f ticks/s)\n" t wtime (fromIntegral wtime / t)

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
