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
/----\
|    |
|    |
\----/
|]

t2 = [here|
/-----\
|     |
|  /--+--\
|  |  |  |
\--+--/  |
   |     |
   \-----/
|]

-- |  |  |  |  |
-- v  |  |  |  |
-- |  v  v  |  |
-- |  |  |  v  X
-- |  |  ^  ^  |
-- ^  ^  |  |  |
-- |  |  |  |  |
t3 = [here|
|
v
|
|
|
^
|
|]

t4 = [here|
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
|]

t5 = [here|
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
|]
-- /-->\        
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \->--/
--   \------/   

-- /---v        
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \-+>-/
--   \------/   

-- /---\        
-- |   v  /----\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \-+->/
--   \------/   

-- /---\        
-- |   |  /----\
-- | /->--+-\  |
-- | | |  | |  |
-- \-+-/  \-+--^
--   \------/   

-- /---\        
-- |   |  /----\
-- | /-+>-+-\  |
-- | | |  | |  ^
-- \-+-/  \-+--/
--   \------/   

-- /---\        
-- |   |  /----\
-- | /-+->+-\  ^
-- | | |  | |  |
-- \-+-/  \-+--/
--   \------/   

-- /---\        
-- |   |  /----<
-- | /-+-->-\  |
-- | | |  | |  |
-- \-+-/  \-+--/
--   \------/   

-- /---\        
-- |   |  /---<\
-- | /-+--+>\  |
-- | | |  | |  |
-- \-+-/  \-+--/
--   \------/   

-- /---\        
-- |   |  /--<-\
-- | /-+--+-v  |
-- | | |  | |  |
-- \-+-/  \-+--/
--   \------/   

-- /---\        
-- |   |  /-<--\
-- | /-+--+-\  |
-- | | |  | v  |
-- \-+-/  \-+--/
--   \------/   

-- /---\        
-- |   |  /<---\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \-<--/
--   \------/   

-- /---\        
-- |   |  v----\
-- | /-+--+-\  |
-- | | |  | |  |
-- \-+-/  \<+--/
--   \------/   

-- /---\        
-- |   |  /----\
-- | /-+--v-\  |
-- | | |  | |  |
-- \-+-/  ^-+--/
--   \------/   

-- /---\        
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  X |  |  -- 7,3
-- \-+-/  \-+--/
--   \------/   

t6 = [here|
/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/
|]

type X = Int
type Y = Int
type Dir = Char
carts = "^>v<"
dirs = carts ++ "X+"  -- X means crash/stop, + means pick the next intersection choice
type Cart = ((X, Y), Dir, Int)  -- position, heading, number of intersections passed
type T = Int  -- ticks since simulation start. When negative, indicates the simulation should stop.
type Track = [String]
type World = (Track, T, [Cart])

parse :: String -> World
parse s = (ss', 0, carts)
  where
    ss = lines s
    carts = sortcarts $ concatMap rowcarts $ zip [0..] ss
    ss' = map fillrow ss

isfinished :: World -> Bool
isfinished (_,t,_) | t < 0 = True
isfinished _ = False

rowcarts (y,s) = [((x,y), d, 0) | (x,d) <- filter ((`elem` carts).snd) $ zip [0..] s]

sortcarts = sortOn ((\(a,b) -> (b,a)).first3)

fillrow = map fillcell
  where
    fillcell '^' = '|'
    fillcell 'v' = '|'
    fillcell '>' = '-'
    fillcell '<' = '-'
    fillcell c   = c

iscrashed :: Cart -> Bool
iscrashed (_,'X',_) = True
iscrashed _ = False

update :: Bool -> World -> IO World
update part2 oldw@(track, t, carts) = do
  let
    t' = t+1
    carts' = foldl' (move part2 track) carts $ sortcarts carts
    neww = (track, t', carts')
    endw = (track, -t', carts')

  case carts' of

    cs | not part2 && any iscrashed cs -> do
      let c = fromJust $ find iscrashed cs
      printworld neww
      printf "first crash happens in tick %d: %s\n" t' (show c)
      return endw

    [c] | part2 -> do
      printworld neww
      printf "last cart standing after tick %d: %s\n" t' (show $ first3 c)
      return endw

    [] | part2 -> do
      printworld neww
      printf "no carts left after tick %d\n" t'
      return endw

    _ | neww == oldw -> do
      printworld neww
      printf "track has stabilised after tick %d\n" t'
      return endw

    _ ->
      return neww

dirdelta '^' = (0,-1)
dirdelta 'v' = (0,1)
dirdelta '<' = (-1,0)
dirdelta '>' = (1,0)
dirdelta _   = (0,0)

peek track (x,y) = track !! y !! x

-- move the given cart, possibly crashing it and other affected carts.
move :: Bool -> Track -> [Cart] -> Cart -> [Cart]
move part2 _     cs    (_,'X',_)        = cs  -- already crashed carts do nothing
move part2 track carts cart@((x,y),d,i) =
  (if part2 then filter ((/='X').second3) else id) $
  cart' : map (maybecrashothers cart') (carts \\ [cart])
  where
    (dx,dy) = dirdelta d
    (ax,ay) = (x+dx,y+dy)
    trackahead = track `peek` (ax,ay)
    cartsahead  = filter ((==(ax,ay)).first3) carts
    cart' =
      case (cartsahead, lookup (d,trackahead) transitions) of
        ((_:_), _)      -> ((ax,ay),'X',i)
        ([],  Just 'X') -> ((x,y),'X',i)
        ([],  Just '+') -> ((ax,ay),nextintersectiondir i d,i+1)
        ([],  Just c)   -> ((ax,ay),c,i)
        ([],  Nothing)  -> ((x,y),'X',i)
    maybecrashothers (loc1,'X',_) (loc,_,i) | loc1==loc = (loc,'X',i)
    maybecrashothers _ c = c
          
transitions = [
  (('^','|'),'^'),
  (('v','|'),'v'),
  
  (('>','-'),'>'),
  (('<','-'),'<'),
  
  (('^','/'),'>'),
  (('v','/'),'<'),
  (('>','/'),'^'),
  (('<','/'),'v'),
  
  (('^','\\'),'<'),
  (('v','\\'),'>'),
  (('>','\\'),'v'),
  (('<','\\'),'^'),
  
  (('^','+'),'+'),
  (('v','+'),'+'),
  (('>','+'),'+'),
  (('<','+'),'+')
  ]

nextintersectiondir :: Int -> Dir -> Dir
nextintersectiondir i d = turn d
  where
    turn :: Dir -> Dir
    turn = [leftturn,noturn,rightturn] !! (i `mod` 3)

leftturn :: Dir -> Dir
leftturn d = carts !! i'
  where
    i = fromJust (d `elemIndex` carts) - 1
    i' = if i < 0 then 3 else i

noturn :: Dir -> Dir
noturn d = d

rightturn :: Dir -> Dir
rightturn d = carts !! i'
  where
    i = fromJust (d `elemIndex` carts) + 1
    i' = if i > 3 then 0 else i

printnothing :: World -> IO ()
printnothing = const $ return ()

printcartbounds :: World -> IO ()
printcartbounds (_,t,carts) =
  printf "%d carts, bounding box %s\n" (length carts) boundsstr
  where
    ps = map first3 carts
    xs = map fst ps
    ys = map snd ps
    boundsstr | null carts = "(none)"
              | otherwise  = printf "%d x %d" (maximum xs - minimum xs) (maximum ys - minimum ys)

printcarts :: World -> IO ()
printcarts (_,t,carts) =
  printf "%d carts, %s\n" (length carts) (show carts)
  where
    ps = map first3 carts
    xs = map fst ps
    ys = map snd ps
    boundsstr | null carts = "(none)"
              | otherwise  = printf "%d x %d" (maximum xs - minimum xs) (maximum ys - minimum ys)

printworld :: World -> IO ()
printworld (track,t,carts) =
  printf ("\n%d:\n" ++ unlines (map (drawcarts carts) $ zip [0..] track)) t
  where
    drawcarts carts (y,s) = map (maybedrawcart carts) $ zip [0..] s
      where
        maybedrawcart carts (x,c) =
          case filter ((==(x,y)).first3) carts of
            (_,d,_):_ -> d
            _         -> c

-- display current tick, cart bounds, and as much of the world as will fit,
-- in ansi terminal, and pause for the given delay.
displayWorldAnsi :: Bool -> Double -> World -> IO ()
displayWorldAnsi showtrack delaysecs w@(track,t,carts) = do
  Just (Window{..}) <- size
  let
    maxy = height - 3
    maxx = width - 3

  clearScreen
  setCursorPosition 0 0

  setSGR [
     SetColor Foreground Vivid Green
    ,SetColor Background Dull Black
    ,SetConsoleIntensity BoldIntensity
    ,SetSwapForegroundBackground False
    ]
  putStr $ "t " ++ show t ++ "  "
  printcartbounds w

  when showtrack $ do
    setSGR [
       SetColor Foreground Dull Red
      ,SetColor Background Dull Black
      ,SetConsoleIntensity FaintIntensity
      ,SetSwapForegroundBackground False
      ]
    putStrLn $ unlines $ map (take maxx) $ take maxy track

  setSGR [
     SetColor Foreground Vivid White
    ,SetColor Background Dull Black
    ,SetConsoleIntensity BoldIntensity
    ,SetSwapForegroundBackground True
    ]
  forM_ carts $ \((x,y),d,_) -> do
    -- when (y < maxy && x < maxx) $ do
      setCursorPosition (y+1) x
      putChar d

  threadDelay $ round $ delaysecs * 1e6

initterm = do
  hSetBuffering stdout NoBuffering
  hideCursor

resetterm = do
  setSGR [Reset]
  showCursor

displaycommands = [
   ("printnothing"    ,printnothing)
  ,("printcarts"      ,printcarts)
  ,("printcartbounds" ,printcartbounds)
  ,("printworld"      ,printworld)
  ,("displaycarts",    displayWorldAnsi False 0.001)
  ,("displayworld",    displayWorldAnsi True  0.05)
  ]

usage = "Usage: ./13 INPUTFILE " ++ intercalate "|" (map fst displaycommands)

main = do
  
  args <- getArgs
  when (null args) $ putStrLn usage >> exitSuccess
  let
    [f,displaytype] = take 2 $ args ++ drop (length args) ["13.in","printnothing"]
    -- w4 = parse t4
    displayfn =
      fromMaybe (error $ "bad display type. "++usage) $ lookup displaytype displaycommands
  
    updateAnd :: (World -> IO ()) -> Bool -> World -> IO World
    updateAnd = \display part2 -> update part2 >=> (\w -> display w $> w)
    -- updateAnd = \update display -> update >=> ((<$) <*> display)

  w <- parse <$> readFile f
  
  -- part 1
  -- iterateUntilM isfinished (updateAnd printnothing False) w
    -- first crash after tick 145: (40,90)

  -- part 2
  let bracket = if ("display" `isInfixOf` displaytype)
                then bracket_ initterm resetterm
                else id
  bracket $ iterateUntilM isfinished (updateAnd displayfn True) w
  
    -- last cart standing after tick 8131: ((40,133),'^',3059)
    -- last cart standing after tick 8132: (40,133)
    -- XXX wrong:
    -- 40,132
    -- 40,133 
    -- 40,134
    --
    -- mcpower's input: no carts left after tick 30476
    -- glguy's        : no carts left after tick 30476  
    -- dsal's         : no carts left after tick 7671
