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

-- assert that prints to stdout in normal output sequence
assert_ :: (HasCallStack, MonadIO m) => Bool -> m ()
assert_ True  = return ()
assert_ False = liftIO $ putStrLn " assertion failed" >> exitFailure

-- timeIt :: MonadIO m => m a -> m a
-- Wrap a MonadIO computation so that it prints out the execution time.

-- timeItShow :: (MonadIO m, Show a) => m a -> m a
-- Like timeIt, but uses the show rendering of a as label for the timing.

-- timeItNamed :: MonadIO m => String -> m a -> m a
-- Like timeIt, but uses the String as label for the timing.

-- timeItT :: MonadIO m => m a -> m (Double, a)
-- Wrap a MonadIO computation so that it returns execution time in seconds, as well as the result value.

--

-- t1 = [here|
-- 37
--  |]
-- (3)[7]
-- (3)[7] 1  0 
--  3  7  1 [0](1) 0 
--  3  7  1  0 [1] 0 (1)
-- (3) 7  1  0  1  0 [1] 2 
--  3  7  1  0 (1) 0  1  2 [4]
--  3  7  1 [0] 1  0 (1) 2  4  5 
--  3  7  1  0 [1] 0  1  2 (4) 5  1 
--  3 (7) 1  0  1  0 [1] 2  4  5  1  5 
--  3  7  1  0  1  0  1  2 [4](5) 1  5  8 
--  3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
--  3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6 
--  3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7 
--  3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7 
--  3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9 
--  3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2
-- 
-- If the Elves think their skill will improve after making 9 recipes, the scores of the ten recipes after the first nine on the scoreboard would be 5158916779 (highlighted in the last line of the diagram).
-- After 5 recipes, the scores of the next ten would be 0124515891.
-- After 18 recipes, the scores of the next ten would be 9251071085.
-- After 2018 recipes, the scores of the next ten would be 5941429882.

type T = Int -- simulation time, 0..
type S = Int -- recipe score, 0..9
data W = W { -- world
   wtime    :: T
  ,wscores  :: [S]
  ,welf1    :: S
  ,welf2    :: S
} deriving (Show)

parse :: String -> W
parse s =
  W{wtime   = 0
   ,wscores = map toscore s
   ,welf1   = 0
   ,welf2   = 1
   }

toscore c = ord c - ord '0'

time    n W{..} = wtime == n
recipes n W{..} = length wscores >= n

main :: IO ()
main = do

  -- let (usage,defargs) = ("Usage: ./14 [INPUTFILE]", ["14.in"])
  -- args <- getArgs
  -- -- when (null args) $ putStrLn usage >> exitSuccess
  -- -- let [f] = take 1 $ args ++ drop (length args) defargs
  -- -- input <- parse <$> readFile f

  ss <- fmap (concatMap show) $ nextScores 9 10 return $ parse "37"
  putStr ss >> assert_ (ss == "5158916779") >> putStrLn " ok"

  ss <- fmap (concatMap show) $ nextScores 5 10 return $ parse "37"
  putStr ss >> assert_ (ss == "0124515891") >> putStrLn " ok"

  ss <- fmap (concatMap show) $ nextScores 18 10 return $ parse "37"
  putStr ss >> assert_ (ss == "9251071085") >> putStrLn " ok"

  ss <- fmap (concatMap show) $ nextScores 2018 10 return $ parse "37"
  putStr ss >> assert_ (ss == "5941429882") >> putStrLn " ok"

  let input = "633601"
      wrong = ["9714214618"]
  ss <- fmap (concatMap show) $ nextScores (length input) 10 printworld $ parse input
  putStr ss >> assert_ (not $ ss `elem` wrong) >> putStrLn " ok"

-- part 1 world update function
update1 :: W -> IO W
update1 w@W{..} = do
  let
    (cur1,cur2) = (wscores!!welf1, wscores!!welf2)
    wscores' = wscores ++ (map toscore $ show $ sum [cur1,cur2])
    welf1' = (welf1 + cur1 + 1) `mod` length wscores'
    welf2' = (welf2 + cur2 + 1) `mod` length wscores'
  return w{
     wtime   = wtime+1
    ,wscores = wscores'
    ,welf1   = welf1'
    ,welf2   = welf2'
    }

-- | after the first m scores in the given World, get the next n scores
nextScores :: Int -> Int -> (W -> IO W) -> W -> IO [S]
nextScores afterm nextn display w = do
  w' <- iterateUntilM (recipes (afterm + nextn)) (update1 >=> display) <=< display $ w
  return $ take nextn $ drop afterm $ wscores w'

-- display functions, these return the unmodified World for easier chaining

printworld :: W -> IO W
printworld w@W{..} = do
  printf "%4d (%4d recipes):  %s\n" wtime (length wscores) (concat $ map fmt $ zip [0..] wscores)
  return w
  where
    fmt (i,d)
      | i==welf1 && i==welf2 = "("++show d++"]"
      | i==welf1             = "("++show d++")"
      | i==welf2             = "["++show d++"]"
      | otherwise            = " "++show d++" "

