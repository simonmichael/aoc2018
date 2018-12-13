#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
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

s1 = [here|
|  |  |  |  |
v  |  |  |  |
|  v  v  |  |
|  |  |  v  X
|  |  ^  ^  |
^  ^  |  |  |
|  |  |  |  |
|]

t3 = [here|
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
|]

s2 = [here|
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   

/-->\        
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \->--/
  \------/   

/---v        
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+>-/
  \------/   

/---\        
|   v  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+->/
  \------/   

/---\        
|   |  /----\
| /->--+-\  |
| | |  | |  |
\-+-/  \-+--^
  \------/   

/---\        
|   |  /----\
| /-+>-+-\  |
| | |  | |  ^
\-+-/  \-+--/
  \------/   

/---\        
|   |  /----\
| /-+->+-\  ^
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /----<
| /-+-->-\  |
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /---<\
| /-+--+>\  |
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /--<-\
| /-+--+-v  |
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /-<--\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /<---\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-<--/
  \------/   

/---\        
|   |  v----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \<+--/
  \------/   

/---\        
|   |  /----\
| /-+--v-\  |
| | |  | |  |
\-+-/  ^-+--/
  \------/   

/---\        
|   |  /----\
| /-+--+-\  |
| | |  X |  |
\-+-/  \-+--/
  \------/   
|]  -- 7,3

t4 = [here|
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
type T = Int
type Track = [String]
type World = (Track, T, [Cart])

parse :: String -> World
parse s = (ss', 0, carts)
  where
    ss = lines s
    carts = sortcarts $ concatMap rowcarts $ zip [0..] ss
    ss' = map fillrow ss

rowcarts (y,s) = [((x,y), d, 0) | (x,d) <- filter ((`elem` carts).snd) $ zip [0..] s]

sortcarts = sortOn ((\(a,b) -> (b,a)).first3)

fillrow = map fillcell
  where
    fillcell '^' = '|'
    fillcell 'v' = '|'
    fillcell '>' = '-'
    fillcell '<' = '-'
    fillcell c   = c

update (track, t, carts) =
  case carts' of
    [c] -> error ("last cart standing after tick "++show t++": "++show c)  -- part 2
    _   -> (track, t+1, sortcarts carts')
  where
    carts' = foldl' (move track) carts $ sortcarts carts

dirdelta '^' = (0,-1)
dirdelta 'v' = (0,1)
dirdelta '<' = (-1,0)
dirdelta '>' = (1,0)
dirdelta _   = (0,0)

peek track (x,y) = track !! y !! x

-- move the given cart, possibly crashing it and other affected carts.
move :: Track -> [Cart] -> Cart -> [Cart]
move _     cs    (_,'X',_)        = cs  -- already crashed carts do nothing
move track carts cart@((x,y),d,i) =
  filter ((/='X').second3) $  -- part 2
  cart' : map (maybecrashothers cart') (carts \\ [cart])
  where
    (dx,dy) = dirdelta d
    (ax,ay) = (x+dx,y+dy)
    trackahead = track `peek` (ax,ay)
    cartsahead  = filter ((==(ax,ay)).first3) carts
    cart' =
      case (cartsahead, lookup (d,trackahead) transitions) of
        ((_:_), _)      -> ((ax,ay),'X',i) -- & error ("first crash:"++show (ax,ay))
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

printworld (track,t,carts) =
  printf ("\n%d:\n" ++ unlines (map (drawcarts carts) $ zip [0..] track)) t
  where
    drawcarts carts (y,s) = map (maybedrawcart carts) $ zip [0..] s
      where
        maybedrawcart carts (x,c) =
          case filter ((==(x,y)).first3) carts of
            (_,d,_):_ -> d
            _         -> c

main = do
  -- mapM_ printworld $ take 20 $ iterate update $ parse t3
  -- iterateUntilM ((==n).fst) (nextgen verbosity rules n) g0

  input <- readFile "13.in"
  -- part 1
  -- mapM_ printworld $ iterate update $ parse input -- 40,90
  -- part 2
  -- mapM_ (pp.length.third3) $ iterate update $ parse t4  -- 6,4
  mapM_ (pp.length.third3) $ iterate update $ parse input  --
{- not:
40,132
40,133 
40,134 (?)
-}
