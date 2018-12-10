#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package containers
   --package here
   --package megaparsec
   --package mtl
   --package pretty-show
   --package time
   --package safe
   --package scanf
   --package split
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
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
import Safe
import System.Environment
import Text.Printf
import Text.Show.Pretty

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow

ltrace :: Show a => String -> a -> a
ltrace msg x =
  trace (msg++": "++show x)
  x

iterateN :: Int -> (a -> a) -> (a -> a)
iterateN n f = (!! n) . iterate f   -- n must be positive
-- iterateN n f = foldr (.) id $ replicate n f  -- negative n is silently equivalent to 0

first3 (x,_,_) = x
second3 (_,x,_) = x
third3 (_,_,x) = x

first4 (x,_,_,_) = x
second4 (_,x,_,_) = x
third4 (_,_,x,_) = x
fourth4 (_,_,_,x) = x

{-

You talk to the Elves while you wait for your navigation system to
initialize. To pass the time, they introduce you to their favorite
marble game.

The Elves play this game by taking turns arranging the marbles in a
circle according to very particular rules. The marbles are numbered
starting with 0 and increasing by 1 until every marble has a number.

First, the marble numbered 0 is placed in the circle. At this point,
while it contains only a single marble, it is still a circle: the
marble is both clockwise from itself and counter-clockwise from
itself. This marble is designated the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining
marble into the circle between the marbles that are 1 and 2 marbles
clockwise of the current marble. (When the circle is large enough,
this means that there is one marble between the marble that was just
placed and the current marble.) The marble that was just placed then
becomes the current marble.

However, if the marble that is about to be placed has a number which
is a multiple of 23, something entirely different happens. First, the
current player keeps the marble they would have placed, adding it to
their score. In addition, the marble 7 marbles counter-clockwise from
the current marble is removed from the circle and also added to the
current player's score. The marble located immediately clockwise of
the marble that was removed becomes the new current marble.

For example, suppose there are 9 players. After the marble with value
0 is placed in the middle, each player (shown in square brackets)
takes a turn. The result of each of those turns would produce circles
of marbles like this, where clockwise is to the right and the
resulting current marble is in parentheses:

[-] (0)
[1]  0 (1)
[2]  0 (2) 1 
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3 
[5]  0  4  2 (5) 1  3 
[6]  0  4  2  5  1 (6) 3 
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7 
[9]  0  8  4 (9) 2  5  1  6  3  7 
[1]  0  8  4  9  2(10) 5  1  6  3  7 
[2]  0  8  4  9  2 10  5(11) 1  6  3  7 
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7 
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7 
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7 
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15 
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15 
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15 
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15 
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15 
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15 
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last
marble is used up. Assuming the example above ends after the marble
numbered 25, the winning score is 23+9=32 (because player 5 kept
marble 23 and removed marble 9, while no other player got any points
in this very short example game).

Here are a few more examples:

    10 players; last marble is worth 1618 points: high score is 8317
    13 players; last marble is worth 7999 points: high score is 146373
    17 players; last marble is worth 1104 points: high score is 2764
    21 players; last marble is worth 6111 points: high score is 54718
    30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?

-}
      
test = map parse $ lines [here|
10 players; last marble is worth 1618 points: high score is 8317
13 players; last marble is worth 7999 points: high score is 146373
17 players; last marble is worth 1104 points: high score is 2764
21 players; last marble is worth 6111 points: high score is 54718
30 players; last marble is worth 5807 points: high score is 37305
|]

parse :: String -> (Int,Int,Int)
parse = (\[a,b,c] -> (read a,read b,read c)) . words . map (\c->if isDigit c then c else ' ')

type M = Int  -- marble, 0..
data C = C {
   cmarbles    :: !(S.Seq Int)
  ,cindex      :: !Int
  ,cnextmarble :: !M
  }
type Player = Int
type Score = Int
type G = (C, M.Map Player Score)  -- game: circle, and each player's score so far

-- initial circle
empty :: C
empty = C (S.fromList [0]) 0 1

-- move current index clockwise by one place,
-- assuming the circle is enlarged by one because we are about to add.
movec :: C -> C
movec c@C{..}
  | S.null cmarbles = c
  | otherwise = c{cindex=((cindex `mod` l) + 1) `mod` (l + 1)}
  where
    l = length cmarbles

-- move current index anticlockwise by one place.
movea :: C -> C
movea c@C{..}
  | S.null cmarbles = c
  | cindex==0 = c{cindex = length cmarbles - 1}
  | otherwise = c{cindex = cindex - 1}

-- move current index N places clockwise (N>0) or anticlockwise (N<0).
move :: Int -> C -> C
move n = iterateN (abs n) (if n>=0 then movec else movea)

-- insert the next marble at the circle's current index
add :: C -> C
add c@C{..} = c{cmarbles = S.insertAt cindex cnextmarble cmarbles}

forceSpine :: [a] -> ()
forceSpine = foldr (const id) ()

forceAllElementsWHNF :: [a] -> ()
forceAllElementsWHNF = foldr seq ()


-- remove the marble at the circle's current index, and also return it
remove :: C -> (C,M)
remove c@C{..} =
  ( c{cmarbles = S.deleteAt cindex cmarbles}, fromJust $ S.lookup cindex cmarbles )

-- increment the circle's next marble number
next :: C -> C
next c@C{..} = c{cnextmarble = cnextmarble + 1}

-- add the circle's next marble, and also return any score from this move.
place :: C -> (C, Int)
place c@C{..}
  | cnextmarble `mod` 23 /= 0 =
      (next $ add $ move 2 c, 0)
  | otherwise =
      let
        (c',r) = remove $ move (-7) c
      in
        (next $ c', cnextmarble + r)

initGame :: Int -> G
initGame players = (empty, M.fromList (zip [1..players] (repeat 0)))

-- which player plays the given turn, both 1-based.
turnPlayer numplayers turn =
  case turn `mod` numplayers of
    0 -> numplayers
    n -> n

-- perform the next player's move, incrementing their score if applicable.
doTurn :: Int -> G -> G
doTurn verbosity (c@C{..}, ps) =
  let
    numplayers = M.size ps
    turn = cnextmarble
    player = turnPlayer numplayers turn
    (c',s) = place c
    ps' | s==0 = ps
        | otherwise =
            (if verbosity>0 then trace (printf "turn %4d %3s%s   P%d scored %d" turn (printf "P%d" player :: String) (concat $ showScores ps'') player s) else id)
            ps''
        where ps'' = M.insertWith (+) player s ps
  in
    (if verbosity>1 then traceGame else id)
    (c',ps')

-- print a one-line summary of current state
traceGame :: G -> G
traceGame g@(c@C{..}, ps) = flip trace g $
  concat $ concat [
     [printf "turn %4d %3s" turn (printf "P%d" player :: String)]
    ,showScores ps
    ,["   ["]
    ,map (printf "%4s" . showm) $ zip [0..] $ toList cmarbles
    ,["]"]
    ]
  where
    numplayers = M.size ps
    turn = cnextmarble - 1
    player = turnPlayer numplayers turn
    showm (mi,m) | mi==cindex = "("++show m++")"
                 | otherwise  = " "++show m++" "

showScores :: M.Map Player Score -> [String]
showScores = map (printf "%7d") . M.elems

runGame :: Int -> Int -> Int -> IO ()
runGame numplayers nummarbles verbosity = do
  t0 <- getCurrentTime
  let (_,ps) = (iterate' (doTurn verbosity) $ initGame numplayers) !! nummarbles
  t1 <- ps `seq` getCurrentTime
  let mins = fromRational $ toRational $ diffUTCTime t1 t0 :: Double
  printf "after %d players playing %d marbles, high score is %d\n"
    numplayers nummarbles (maximum $ M.elems ps)
  printf "%d marbles processed in %.2fs (%.1f marbles/s)\n"
    nummarbles mins (fromIntegral nummarbles / mins / 1)

main = do
  args <- getArgs
  case args of
    (numplayers:nummarbles:rest) -> 
      runGame (read numplayers) (read nummarbles) (read $ headDef "0" rest)
    _ ->
      -- forM_ test $ \(numplayers,nummarbles,_expectedscore) ->
      --   runGame (numplayers) (nummarbles) 0
      forM_ input $ \(numplayers,nummarbles,_expectedscore) ->
        runGame (numplayers) (nummarbles) 0
        -- 386018

{-

Amused by the speed of your answer, the Elves are curious:

What would the new winning Elf's score be if the number of the last
marble were 100 times larger?

-}

-- ./09 476 7165700

input = map parse $ lines [here|
476 players; last marble is worth 71657 points. score ? (0)
|]
