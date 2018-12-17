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
import Data.Array (Array,array,listArray,(!),(//))
import Data.Bits
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
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
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

-- type Ticks = Int -- simulation time, 0..
-- type Seconds = Double  -- real world time
-- type Delay = Seconds  -- a length of time to pause; negative means wait for input
-- type X = Int
-- type Y = Int
-- type P = (X,Y)
-- data W = W { -- simulation world
--    wtime    :: Ticks
--   ,wend     :: Maybe String  --  reason for ending
--   ,wregs    :: M
-- } deriving (Eq,Show)

type RegisterNum = Int  -- 0..3
type M = Array RegisterNum Int  -- machine (registers) state

data I = I {  -- instruction
   iop  :: Opcode
  ,iia  :: Int        -- input a
  ,iib  :: Int        -- input b
  ,ioc  :: RegisterNum -- output c
} deriving (Eq,Show)

data U = U {  -- unknown instruction
   un  :: OpcodeNum
  ,ua  :: Int        -- input a
  ,ub  :: Int        -- input b
  ,uc  :: RegisterNum -- output c
} deriving (Eq,Show)

type OpcodeNum = Int  -- 0..15, corresponds to Opcodes but we don't know which

data Opcode =
    Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Show,Eq,Enum,Ord,Bounded)

data E = E { -- example
   eregs1 :: M
  ,einstr :: U
  ,eregs2 :: M
} deriving (Show)

-- Before: [3, 2, 1, 1]
-- 9 2 1 2
-- After:  [3, 2, 2, 1]
-- 
parseexample :: HasCallStack => String -> E
parseexample s =
  let
    l1:l2:l3:_ = lines s
    rs1 = read $ drop 8 l1
    [n,a,b,c] = map read $ words l2
    rs2 = read $ drop 8 l3
  in
    E{eregs1 = listArray (0,3) rs1
     ,einstr = U n a b c
     ,eregs2 = listArray (0,3) rs2
    }

parseunknowninstruction s = U n a b c where [n,a,b,c] = map read $ words s

parseinput :: HasCallStack => String -> ([E],[U])
parseinput s =
  let
    [e,u] = splitOn "\n\n\n\n" s
    es    = map parseexample $ map unlines $ chunksOf 4 $ lines e
    us    = map parseunknowninstruction $ lines u
  in
    (es,us)

exec :: I -> M -> M
exec (I Addr a b c) m = m//[(c, m!a+m!b)]                     -- (add register) stores into register C the result of adding register A and register B.
exec (I Addi a b c) m = m//[(c, m!a+b)]                       -- (add immediate) stores into register C the result of adding register A and value B.
exec (I Mulr a b c) m = m//[(c, m!a*m!b)]                     -- (multiply register) stores into register C the result of multiplying register A and register B.
exec (I Muli a b c) m = m//[(c, m!a*b)]                       -- (multiply immediate) stores into register C the result of multiplying register A and value B.
exec (I Banr a b c) m = m//[(c, (m!a) .&. (m!b))]             -- (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
exec (I Bani a b c) m = m//[(c, (m!a) .&. b)]                 -- (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
exec (I Borr a b c) m = m//[(c, (m!a) .|. (m!b))]             -- (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
exec (I Bori a b c) m = m//[(c, (m!a) .|. b)]                 -- (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
exec (I Setr a _ c) m = m//[(c, (m!a))]                       -- (set register) copies the contents of register A into register C. (Input B is ignored.)
exec (I Seti a _ c) m = m//[(c, a)]                           -- (set immediate) stores value A into register C. (Input B is ignored.)
exec (I Gtir a b c) m = m//[(c, if a > m!b then 1 else 0)]    -- (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
exec (I Gtri a b c) m = m//[(c, if m!a > b then 1 else 0)]    -- (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
exec (I Gtrr a b c) m = m//[(c, if m!a > m!b then 1 else 0)]  -- (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
exec (I Eqir a b c) m = m//[(c, if a == m!b then 1 else 0)]   -- (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
exec (I Eqri a b c) m = m//[(c, if m!a == b then 1 else 0)]   -- (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
exec (I Eqrr a b c) m = m//[(c, if m!a == m!b then 1 else 0)] -- (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

-- which opcodes have a behaviour matching this example ?
opcodesbehavinglike :: E -> [Opcode]
opcodesbehavinglike E{..} =
  [o | o <- [minBound..maxBound], exec (einstr `withopcode` o) eregs1 == eregs2]
    
withopcode :: U -> Opcode -> I
withopcode (U _ a b c) o = I o a b c

-- for each opcode number used in these examples, based on their behaviour
-- in these examples, which opcodes do they possibly correspond to ?
candidatesbynum :: [E] -> [(OpcodeNum, [Opcode])]
candidatesbynum examples = opsbynum
  where
    eopcodenum = un.einstr
    esbynum = [ (eopcodenum $ head es, es)
              | es <- groupBy ((==) `on` eopcodenum) $ sortOn eopcodenum examples
              ]
    opsbynum =
      [(n,opssatisfyingall)
      | (n, es) <- esbynum
      , let opsforeachex = [opcodesbehavinglike e | e <- es]
      , let allops = nub $ sort $ concat opsforeachex
      , let opssatisfyingall = [o | o <- allops, (o `elem`) `all` opsforeachex]
      ]

-- given a set of examples of behaviour of unknown numbered opcodes, and knowing the
-- the opcodes that exist and their behaviour, match up as many as possible
-- of the opcode numbers and opcodes.
-- Returns the identified opcodes and any remaining opcode numbers & candidate opcodes
-- that could not be resolved further.
identifyopcodes :: [E] -> ([(OpcodeNum,Opcode)], [(OpcodeNum,[Opcode])])
identifyopcodes examples = (sort identified, sort remaining)
  where
    (identified, remaining) = identifyopcodes' ([], candidatesbynum examples)

    -- identify opcode numbers with only one possible opcode candidate,
    -- then remove that opcode from the candidates and try to identify
    -- the remaining opcode numbers from the remaining candidates,
    -- recursively until we can go no farther.
    identifyopcodes' :: ([(OpcodeNum,Opcode)],[(OpcodeNum,[Opcode])]) -> ([(OpcodeNum,Opcode)], [(OpcodeNum,[Opcode])])
    identifyopcodes' (identified, candidates) =
      let
        identifiedopcodes = map snd identified
        candidatesrefined = nub $ sort [(n, os') | (n,os) <- candidates, let os' = os \\ identifiedopcodes, not $ null os']
        (surematches, candidatesremaining) = partition ((==1).length.snd) candidatesrefined
        surematchespairs = [(n,o) | (n,[o]) <- surematches]
        identified' = identified ++ surematchespairs
      in
        if null candidatesremaining || null surematches
        then (identified', candidatesremaining)
        else identifyopcodes' (identified', candidatesremaining)

-- main

main :: HasCallStack => IO ()
main = do
  args <- getArgs
  let progname = "16"
  let (usage, defargs) = ("Usage: "++progname++" [INPUTFILE]", [progname++".in"])
  let [f] = take 1 $ args ++ drop (length args) defargs
  input <- readFile f

  -- part 1
  let (examples, instructions) = parseinput input
  pp $ length $ filter ((>=3).length) $ map opcodesbehavinglike examples
  -- 493

  -- part 2
  let (opcodeassocs,[]) = identifyopcodes examples
      opcode n = fromJust $ lookup n opcodeassocs
      execu :: U -> M -> M
      execu (U n a b c) = exec (I (opcode n) a b c)
      regs1 = listArray (0,3) [0,0,0,0]
      regs2 = foldl' (flip execu) regs1 instructions
  pp $ regs2 ! 0
  -- 445
