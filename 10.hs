#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package "ansi-terminal containers here megaparsec mtl pretty-show safe scanf split time"
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.State
import Debug.Trace
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as S
import Data.String.Here
import Data.Time.Calendar
import Data.Time.Clock
import Debug.Trace
import System.Console.ANSI
import System.Environment
import System.IO
import System.Exit
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf
import qualified Text.Scanf as SC (fmt,printf,scanf)
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


{-
see 10.txt
KBJHEZCB
10369      
-}

test = [here|
position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>
|] & parsePoints

parsePoints s = s
  & lines
  & map (SC.scanf [SC.fmt|position=<%d,  %d> velocity=<%d, %d>|])
  & catMaybes
  & map (SC.printf [SC.fmt|((%d,%d), (%d,%d))|])
  & map read :: [Point]

testresults = [here|
........#.............
................#.....
.........#.#..#.......
......................
#..........#.#.......#
...............#......
....#.................
..#.#....#............
.......#..............
......#...............
...#...#.#...#........
....#..#..#.........#.
.......#..............
...........#..#.......
#...........#.........
...#.......#..........

......................
......................
..........#....#......
........#.....#.......
..#.........#......#..
......................
......#...............
....##.........#......
......#.#.............
.....##.##..#.........
........#.#...........
........#...#.....#...
..#...........#.......
....#.....#.#.........
......................
......................

......................
......................
......................
..............#.......
....#..#...####..#....
......................
........#....#........
......#.#.............
.......#...#..........
.......#..#..#.#......
....#....#.#..........
.....#...#...##.#.....
........#.............
......................
......................
......................

......................
......................
......................
......................
......#...#..###......
......#...#...#.......
......#...#...#.......
......#####...#.......
......#...#...#.......
......#...#...#.......
......#...#...#.......
......#...#..###......
......................
......................
......................
......................

......................
......................
......................
............#.........
........##...#.#......
......#.....#..#......
.....#..##.##.#.......
.......##.#....#......
...........#....#.....
..............#.......
....#......#...#......
.....#.....##.........
...............#......
...............#......
......................
......................
|]
  & splitOn "\n\n"
  & map (S.fromList . map S.fromList . lines) :: [Sky]

type X = Int
type Y = Int
type VX = Int
type VY = Int
type Point = ((X,Y),(VX,VY))

type Sky = S.Seq (S.Seq Char)

skyw = 22
skyh = 16

defsky = S.replicate skyh $ S.replicate skyw '.'

sky w h = S.replicate h $ S.replicate w '.'

ox =skyw`div`2
oy =skyh`div`2

myx = ox-5
myy = oy-4

printSky :: Sky -> IO ()
printSky = putStr . unlines . map toList . toList

printSkies :: [Sky] -> IO ()
printSkies = mapM_ (\(i,s) -> print i >> printSky s) . zip [0..]

pointsDimensions :: [Point] -> (Int,Int)
pointsDimensions ps = (xhi - xlo, yhi - ylo)
  where
    (xs,ys)   = (map (fst . fst) ps, map (snd . fst) ps)
    (xlo,xhi) = (minimum xs, maximum xs)
    (ylo,yhi) = (minimum ys, maximum ys)

pointsTopLeft :: [Point] -> (Int,Int)
pointsTopLeft ps = (minimum xs, minimum ys)
  where
    (xs,ys)   = (map (fst . fst) ps, map (snd . fst) ps)

drawPoints :: Double -> (Int,Int) -> [Point] -> Sky
drawPoints extrascale (translatex,translatey) ps =
  let
    (w,h) = pointsDimensions ps
    xscale = fromIntegral draww / (fromIntegral w) * extrascale
    yscale = fromIntegral drawh / (fromIntegral h) * extrascale
    (x,y) = pointsTopLeft ps
    s = sky draww drawh
  in
    snd $ flip runState s $ do
      mapM_ (drawPoint . scalePoint xscale yscale . translatePoint translatex translatey) ps
      get

draww = 160
drawh = 40

translatePoint :: Int -> Int -> Point -> Point
translatePoint dx dy ((x,y),v) = ((x+dx,y+dy),v)

scalePoint :: Double -> Double -> Point -> Point
scalePoint sx sy ((x,y),(vx,vy)) =
  ((scale sx x, scale sy y),(scale sx vx, scale sy vy))
  where
    scale s x = round $ s * fromIntegral x

drawPoint :: Point -> State Sky ()
drawPoint ((x,y),(_,_)) = do
  s <- get
  put $ S.adjust' (S.update (myx+x) '#') (myy+y) s

updatePoints :: Int -> [Point] -> [Point]
updatePoints vscale = map (movePoint vscale)

movePoint :: Int -> Point -> Point
movePoint vscale ((x,y),(vx,vy)) = ((x+vx*vscale,y+vy*vscale),(vx,vy))

-- t0 = head testresults
-- p0 = test
-- pss = take 4 $ iterate updatePoints p0

main1 = do
  -- putStrLn "t0"
  -- printSky t0
  -- putStrLn "p0"
  -- printSky $ drawPoints p0
  -- printSkies $ map drawPoints $ p0:pss

  [inputfile,vscale',startidx'] <- getArgs
  let [vscale,startidx] = map read [vscale',startidx']
  input <- readFile inputfile
  let
    ps0 = parsePoints input
    pss = iterate' (updatePoints vscale) ps0
    s = 1      -- drawing scale
    t = (0,0)  -- drawing translation
    present i s t@(tx,ty) = do
      clearScreen
      setCursorPosition 0 0
      let ps = pss !! i
      printSky $ drawPoints s t ps
      printf "time:%4d  scale:%10.6f  translate:%s  points dimensions:%s\n" i s (show t) (show $ pointsTopLeft ps)
      putStrLn $ intercalate "  " [
         "j/k:next/prev"
        ,"J/K:next/prev fast"
        ,"0:reset time"
        ,"-/+:zoom"
        ,"1:reset zoom"
        ,"w/x/a/d:pan"
        ,"W/X/A/D:pan fast"
        ,"q: quit"
        ]
      c <- getChar
      case c of
        'q' -> exitSuccess
        -- prev/next
        'k' -> present (max 0 (i-1))   s t
        'j' -> present (i+1)           s t
        'K' -> present (max 0 (i-100)) s t
        'J' -> present (i+100)         s t
        '0' -> present 0               s t
        -- scale
        '-' -> present i (s*0.9) t
        '=' -> present i (s*1.1) t
        '1' -> present i 1       t
        -- translation
        'w' -> present i s (tx     ,ty+1   )
        'x' -> present i s (tx     ,ty-1   )
        'a' -> present i s (tx+1   ,ty     )
        'd' -> present i s (tx-1   ,ty     )
        'W' -> present i s (tx     ,ty+1000)
        'X' -> present i s (tx     ,ty-1000)
        'A' -> present i s (tx+1000,ty     )
        'D' -> present i s (tx-1000,ty     )
        --
        _   -> present i s t

  hSetBuffering stdin NoBuffering
  present startidx s t
