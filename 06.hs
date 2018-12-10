#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package "here megaparsec pretty-show split"
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
import Debug.Trace
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import Data.String.Here
import Text.Printf
import Text.Show.Pretty

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow

{-
The device on your wrist beeps several times, and once again you feel
like you're falling.

"Situation critical," the device announces. "Destination
indeterminate. Chronal interference detected. Please specify new
target coordinates."

The device then produces a list of coordinates (your puzzle
input). Are they places it thinks are safe or dangerous? It recommends
you check manual page 729. The Elves did not give you a manual.

If they're dangerous, maybe you can minimize the danger by finding the
coordinate that gives the largest distance from the other points.

Using only the Manhattan distance, determine the area around each
coordinate by counting the number of integer X,Y locations that are
closest to that coordinate (and aren't tied in distance to any other
coordinate).

Your goal is to find the size of the largest area that isn't
infinite. For example, consider the following list of coordinates:

1, 1
1, 6
8, 3
3, 4
5, 5
8, 9

If we name these coordinates A through F, we can draw them on a grid,
putting 0,0 at the top left:

..........
.A........
..........
........C.
...D......
.....E....
.B........
..........
..........
........F.

This view is partial - the actual grid extends infinitely in all
directions. Using the Manhattan distance, each location's closest
coordinate can be determined, shown here in lowercase:
           
aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf

Locations shown as . are equally far from two or more coordinates, and
so they don't count as being closest to any.

In this example, the areas of coordinates A, B, C, and F are infinite
- while not shown here, their areas extend forever outside the visible
grid. However, the areas of coordinates D and E are finite: D is
closest to 9 locations, and E is closest to 17 (both including the
coordinate's location itself). Therefore, in this example, the size of
the largest area is 17.

What is the size of the largest area that isn't infinite?
-}
      
parse :: String -> (Int,Int)
parse l = (read x, read y) where [x,y] = splitOn ", " l

test = labelPoints $ map parse $ lines $ [here|
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
|]

type Point = (Int,Int)
type LabelledPoint = (Char,(Int,Int))
type Grid = [[Char]]

labelPoints :: [Point] -> [LabelledPoint]
labelPoints = zip ['A'..]

blankGridFrom :: [LabelledPoint] -> Grid
blankGridFrom points = replicate (maxxy+1) $ replicate (maxxy+1) '.'
  where
    maxxy = maximum $ map (fst.snd) points ++ map (snd.snd) points

printGrid :: Grid -> IO ()
printGrid g =
  let
    numberedg = zip [0..] g :: [(Int,[Char])]
  in do
    putStr "    " >> mapM_ (putStr.show) (take (length $ head g) [0..]) >> putStr "\n"
    mapM_ (\(n,l) -> putStr (printf "%3d " n) >> putStrLn l) numberedg

drawPoints :: [LabelledPoint] -> Grid -> Grid
drawPoints ps g =
  let
    numberedlines = zip [0..] g
    linesandpoints = [(l, filter ((==n).snd.snd) ps) | (n,l) <- numberedlines]
    drawLine (l, lps) = foldl' (\l' (c,(x,_)) -> take x l' ++ [c] ++ drop (x+1) l') l lps
  in
    map drawLine linesandpoints

distance :: Point -> LabelledPoint -> Int
distance (x,y) (_,(x',y')) = dx + dy
  where
    dx = if x>x' then x-x' else x'-x
    dy = if y>y' then y-y' else y'-y

closestLabelIfOnlyOne :: [LabelledPoint] -> Point -> Char
closestLabelIfOnlyOne lps (x,y) =
  case
    groupBy ((==) `on` fst) $ reverse $ sortOn fst $
      [(distance (x,y) lp, lp) | lp <- lps]
  of
    ([(_,(c,_))]:_) -> c
    _               -> '.'

drawAreas :: [LabelledPoint] -> Grid -> Grid
drawAreas ps g =
  map drawLine $ zip [0..] g
   where
     drawLine (y,l) = map drawPoint $ zip [0..] l
       where
         drawPoint (x,_) = areaChar $ closestLabelIfOnlyOne ps (x,y)
           where
             areaChar = chr . (+50) . ord

a = do
  let lps = input
  pp lps
  printGrid $
    drawAreas lps $
    blankGridFrom lps
  let
    g = drawAreas lps $ blankGridFrom lps
    edgelabels = nub $ sort $ filter ((/=)'.') $ concat [head g, last g, concatMap (\cs -> [head cs, last cs]) $ drop 1 $ init g]
    finiteareacells = filter (not . (`elem` ('.':edgelabels))) $ concat g
    finiteareasizes = map length $ group $ sort finiteareacells
  pp $ maximum finiteareasizes -- 4284

{-
On the other hand, if the coordinates are safe, maybe the best you can
do is try to find a region near as many coordinates as possible.

For example, suppose you want the sum of the Manhattan distance to all
of the coordinates to be less than 32. For each location, add up the
distances to all of the given coordinates; if the total of those
distances is less than 32, that location is within the desired
region. Using the same coordinates as above, the resulting region
looks like this:

..........
.A........
..........
...###..C.
..#D###...
..###E#...
.B.###....
..........
..........
........F.

In particular, consider the highlighted location 4,3 located at the
top middle of the region. Its calculation is as follows, where abs()
is the absolute value function:

    Distance to coordinate A: abs(4-1) + abs(3-1) =  5
    Distance to coordinate B: abs(4-1) + abs(3-6) =  6
    Distance to coordinate C: abs(4-8) + abs(3-3) =  4
    Distance to coordinate D: abs(4-3) + abs(3-4) =  2
    Distance to coordinate E: abs(4-5) + abs(3-5) =  3
    Distance to coordinate F: abs(4-8) + abs(3-9) = 10
    Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

Because the total distance to all coordinates (30) is less than 32,
the location is within the region.

This region, which also includes coordinates D and E, has a total size of 16.

Your actual region will need to be much larger than this example,
though, instead including all locations with a total distance of less
than 10000.

What is the size of the region containing all locations which have a
total distance to all given coordinates of less than 10000?
-}

pointsNearAllLabelledPoints :: [LabelledPoint] -> Int -> [Point]
pointsNearAllLabelledPoints lps d =
  let
    maxxy = maximum $ map (fst.snd) lps ++ map (snd.snd) lps
    allps = [(x,y) | y<-[0..maxxy], x<-[0..maxxy]]
    nearAll (x,y) = sum (map (distance (x,y)) lps) < d
  in
    filter nearAll allps
  
drawNearAll :: [Point] -> Grid -> Grid
drawNearAll ps g =
  let
    linesandpoints = [(l, filter ((==n).snd) ps) | (n,l) <- zip [0..] g]
    drawLine (l, lps) = foldl' (\l' (x,_) -> take x l' ++ ['#'] ++ drop (x+1) l') l lps
  in
    map drawLine linesandpoints

b = main

main = do
  let
    lps = test
    nearall = pointsNearAllLabelledPoints lps 32
    -- lps = input
    -- nearall = pointsNearAllLabelledPoints lps 10000
  -- pp nearall
  -- printGrid $
  --   drawPoints lps $
  --   drawNearAll nearall $
  --   blankGridFrom lps
  pp $ length nearall

input = labelPoints $ map parse $ lines [here|
337, 150
198, 248
335, 161
111, 138
109, 48
261, 155
245, 130
346, 43
355, 59
53, 309
59, 189
325, 197
93, 84
194, 315
71, 241
193, 81
166, 187
208, 95
45, 147
318, 222
338, 354
293, 242
240, 105
284, 62
46, 103
59, 259
279, 205
57, 102
77, 72
227, 194
284, 279
300, 45
168, 42
302, 99
338, 148
300, 316
296, 229
293, 359
175, 208
86, 147
91, 261
188, 155
257, 292
268, 215
257, 288
165, 333
131, 322
264, 313
236, 130
98, 60
|]
