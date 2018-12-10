#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package "here megaparsec pretty-show safe split"
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
import Control.Monad.State
import Data.Char
import Data.Either
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Here
import Debug.Trace
import Safe
import Text.Printf
import Text.Show.Pretty
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow

ltrace :: Show a => String -> a -> a
ltrace msg x =
  trace (msg++": "++show x)
  x

{-

You find yourself standing on a snow-covered coastline; apparently,
you landed a little off course. The region is too hilly to see the
North Pole from here, but you do spot some Elves that seem to be
trying to unpack something that washed ashore. It's quite cold out, so
you decide to risk creating a paradox by asking them for directions.

"Oh, are you the search party?" Somehow, you can understand whatever
Elves from the year 1018 speak; you assume it's Ancient Nordic
Elvish. Could the device on your wrist also be a translator? "Those
clothes don't look very warm; take this." They hand you a heavy coat.

"We do need to find our way back to the North Pole, but we have higher
priorities at the moment. You see, believe it or not, this box
contains something that will solve all of Santa's transportation
problems - at least, that's what it looks like from the pictures in
the instructions." It doesn't seem like they can read whatever
language it's in, but you can: "Sleigh kit. Some assembly required."

"'Sleigh'? What a wonderful name! You must help us assemble this
'sleigh' at once!" They start excitedly pulling more parts out of the
box.

The instructions specify a series of steps and requirements about
which steps must be finished before others can begin (your puzzle
input). Each step is designated by a single letter. For example,
suppose you have the following instructions:

Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.

Visually, these requirements look like this:


  -->A--->B--
 /    \      \
C      -->D----->E
 \           /
  ---->F-----

Your first goal is to determine the order in which the steps should be
completed. If more than one step is ready, choose the step which is
first alphabetically. In this example, the steps would be completed as
follows:

    Only C is available, and so it is done first.

    Next, both A and F are available. A is first alphabetically, so it
    is done next.

    Then, even though F was available earlier, steps B and D are now
    also available, and B is the first alphabetically of the three.

    After that, only D and F are available. E is not available because
    only some of its prerequisites are complete. Therefore, D is completed
    next.

    F is the only choice, so it is done next.

    Finally, E is completed.

So, in this example, the correct order is CABDFE.

In what order should the steps in your instructions be completed?

-}
      
test = parselines [here|
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
|]

type Step = Char
type Rule = (Step,Step)  -- step a is a prerequisite of step b

parselines :: String -> [Rule]
parselines = fromRight [] . M.parse p ""
  where
    p :: M.Parsec String String [(Char,Char)]
    p = do
      xs <- M.many $ do
        M.string "Step "
        a <- M.letterChar
        M.string " must be finished before step "
        b <- M.letterChar
        M.string " can begin."
        M.optional M.eol
        return (a,b)
      M.eof
      return xs

-- All the steps mentioned in a list of rules.
steps :: [Rule] -> [Step]
steps rules = nub $ sort $ map fst rules ++ map snd rules

-- The steps which have prerequisites.
depsteps rules = nub $ sort $ map snd rules

-- List the steps which have no prequisites.
freesteps rules = steps rules \\ depsteps rules

-- Pick the likely first step from a list of rules, ie the step that
-- has no prerequisites and appears first in the list.
firststep rules = head $ freesteps rules

-- The steps required before this step, if any.
stepdeps :: [Rule] -> Step -> [Step]
stepdeps rules s = map fst $ filter ((==s).snd) rules

-- Given lists of step dependency rules and completed steps,
-- which steps could be done next ?
availablesteps rules done =
  [s | s <- steps rules \\ done,         -- uncompleted steps
    all (`elem` done) $ stepdeps rules s -- whose prequisites have all been completed
  ]

-- Choose the next step (first available one).
donextstep :: [Rule] -> [Step] -> [Step]
donextstep rules done
  | null available = done
  | otherwise      = done++[next]
  where
    cur  = last done
    available = availablesteps rules done
    next = head $ sort $ available

-- Find a complete legal step sequence, hopefully.
findsteps rules =
  last $ take (length $ steps rules) $
  iterate (donextstep rules) [firststep rules]

a = findsteps input

type Time = Int                 -- time in seconds since simulation start
type Worker = Maybe (Step, Time)  -- a worker's current assigned step and predicted completion time
data World = W {
   wrules   :: [Rule]        -- step dependency rules
  ,wtime    :: Time          -- elapsed seconds since simulation start
  ,wtodo    :: [Step]        -- steps remaining to do
  ,wdone    :: [Step]        -- steps done
  ,wworkers :: [Worker]      -- all workers' states
  } deriving (Show)

-- initialise a simulation with given step dependency rules and number of workers
initWorld :: [Rule] -> Int -> World
initWorld rs numworkers =
  W {wrules = rs
    ,wtime  = 0
    ,wdone  = []
    ,wworkers = replicate numworkers Nothing
    ,wtodo = steps rs
    }

-- assign as many tasks as possible to available workers
assignSteps :: World -> World
assignSteps w = w{wworkers=wworkers', wtodo=wtodo'}
  where
    wipsteps       = map fst $ catMaybes $ wworkers w
    readysteps     = -- ltrace ("ready at "++show (wtime w)) $
                     availablesteps (wrules w) (wdone w) \\ wipsteps
    numfreeworkers = length $ filter isNothing $ wworkers w
    assignsteps    = -- ltrace "assigning" $
                     take numfreeworkers readysteps
    wtodo'         = wtodo w \\ assignsteps
    wworkers'      = foldl' assignone (wworkers w) assignsteps
      where
        assignone :: [Worker] -> Step -> [Worker]
        assignone ws s = as ++ [Just (s, finishtime)] ++ drop 1 rest
          where
            (as,rest) = break isNothing ws
            finishtime = wtime w + steptime s

steptime s =
  60 + 
  1 + (ord s - ord 'A')

-- advance the clock to the next task completion(s), record the done task(s),
-- and free up the worker(s).
completeAStep w =
  -- trace ("finished at "++show wtime'++": "++intersperse ' ' finishedsteps)
  w{wtime=wtime',wdone=wdone',wworkers=wworkers'}
  where
    wtime' = minimum $ map snd $ catMaybes $ wworkers w
    isdone (Just (_,finishtime)) | finishtime == wtime' = True
    isdone _ = False
    finishedsteps = map fst $ catMaybes $ filter isdone $ wworkers w
    wdone' = wdone w ++ finishedsteps
    wworkers' = map free $ wworkers w
      where
        free mw | isdone mw = -- trace ("freeing: "++[fst$fromJust$mw])
                              Nothing
                | otherwise = mw

-- print a one-line summary of current simulation state
traceState w@W{..} = flip trace w $ intercalate "  " [
   printf "%4d" wtime
  ,intersperse ' ' $ map (maybe '.' fst) wworkers
  ,wdone
  ]

-- run a simulation until all tasks are completed
runWorld :: World -> World
runWorld w@W{wtodo=[]} = w
runWorld w = w
  & assignSteps
  & traceState
  & completeAStep
  & traceState
  & runWorld

-- simulate completing all tasks with the given dependencies and number of workers,
-- returning the final state
runRules :: [Rule] -> Int -> World
runRules rs numworkers = runWorld $ initWorld rs numworkers

b rs n = wtime (runRules rs n)
-- 1115

{-

As you're about to begin construction, four of the Elves offer to
help. "The sun will set soon; it'll go faster if we work together."
Now, you need to account for multiple people working on steps
simultaneously. If multiple steps are available, workers should still
begin them in alphabetical order.

Each step takes 60 seconds plus an amount corresponding to its letter:
A=1, B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step
Z takes 60+26=86 seconds. No time is required between steps.

To simplify things for the example, however, suppose you only have
help from one Elf (a total of two workers) and that each step takes 60
fewer seconds (so that step A takes 1 second and step Z takes 26
seconds). Then, using the same instructions as above, this is how each
second would be spent:

Second   Worker 1   Worker 2   Done
   0        C          .        
   1        C          .        
   2        C          .        
   3        A          F       C
   4        B          F       CA
   5        B          F       CA
   6        D          F       CAB
   7        D          F       CAB
   8        D          F       CAB
   9        D          .       CABF
  10        E          .       CABFD
  11        E          .       CABFD
  12        E          .       CABFD
  13        E          .       CABFD
  14        E          .       CABFD
  15        .          .       CABFDE

Each row represents one second of time. The Second column identifies
how many seconds have passed as of the beginning of that second. Each
worker column shows the step that worker is currently doing (or . if
they are idle). The Done column shows completed steps.

Note that the order of the steps has changed; this is because steps
now take time to finish and multiple workers can begin multiple steps
simultaneously.

In this example, it would take 15 seconds for two workers to complete
these steps.

With 5 workers and the 60+ second step durations described above, how
long will it take to complete all of the steps?

-}

input = parselines [here|
Step G must be finished before step S can begin.
Step T must be finished before step Q can begin.
Step A must be finished before step B can begin.
Step H must be finished before step X can begin.
Step V must be finished before step O can begin.
Step Z must be finished before step P can begin.
Step R must be finished before step J can begin.
Step L must be finished before step Y can begin.
Step Y must be finished before step E can begin.
Step W must be finished before step X can begin.
Step X must be finished before step B can begin.
Step K must be finished before step E can begin.
Step Q must be finished before step P can begin.
Step U must be finished before step B can begin.
Step M must be finished before step O can begin.
Step P must be finished before step N can begin.
Step I must be finished before step J can begin.
Step B must be finished before step C can begin.
Step C must be finished before step O can begin.
Step J must be finished before step F can begin.
Step F must be finished before step O can begin.
Step E must be finished before step D can begin.
Step D must be finished before step N can begin.
Step N must be finished before step S can begin.
Step S must be finished before step O can begin.
Step W must be finished before step O can begin.
Step L must be finished before step P can begin.
Step N must be finished before step O can begin.
Step T must be finished before step D can begin.
Step G must be finished before step I can begin.
Step V must be finished before step X can begin.
Step B must be finished before step N can begin.
Step R must be finished before step N can begin.
Step H must be finished before step J can begin.
Step B must be finished before step S can begin.
Step P must be finished before step I can begin.
Step A must be finished before step J can begin.
Step A must be finished before step U can begin.
Step B must be finished before step D can begin.
Step T must be finished before step A can begin.
Step U must be finished before step D can begin.
Step T must be finished before step L can begin.
Step I must be finished before step E can begin.
Step R must be finished before step U can begin.
Step H must be finished before step S can begin.
Step P must be finished before step F can begin.
Step Q must be finished before step C can begin.
Step A must be finished before step P can begin.
Step X must be finished before step E can begin.
Step Q must be finished before step N can begin.
Step E must be finished before step N can begin.
Step Q must be finished before step O can begin.
Step J must be finished before step S can begin.
Step X must be finished before step P can begin.
Step K must be finished before step U can begin.
Step F must be finished before step E can begin.
Step C must be finished before step E can begin.
Step H must be finished before step K can begin.
Step W must be finished before step B can begin.
Step G must be finished before step O can begin.
Step F must be finished before step N can begin.
Step I must be finished before step D can begin.
Step G must be finished before step V can begin.
Step E must be finished before step S can begin.
Step Y must be finished before step P can begin.
Step G must be finished before step E can begin.
Step P must be finished before step J can begin.
Step U must be finished before step N can begin.
Step U must be finished before step F can begin.
Step X must be finished before step U can begin.
Step X must be finished before step C can begin.
Step R must be finished before step Q can begin.
Step Q must be finished before step E can begin.
Step Z must be finished before step E can begin.
Step X must be finished before step F can begin.
Step J must be finished before step D can begin.
Step X must be finished before step M can begin.
Step Y must be finished before step D can begin.
Step K must be finished before step J can begin.
Step Z must be finished before step J can begin.
Step M must be finished before step P can begin.
Step T must be finished before step M can begin.
Step F must be finished before step S can begin.
Step P must be finished before step S can begin.
Step X must be finished before step I can begin.
Step U must be finished before step J can begin.
Step M must be finished before step B can begin.
Step Q must be finished before step D can begin.
Step Z must be finished before step I can begin.
Step D must be finished before step S can begin.
Step J must be finished before step N can begin.
Step D must be finished before step O can begin.
Step T must be finished before step H can begin.
Step P must be finished before step D can begin.
Step M must be finished before step F can begin.
Step Y must be finished before step S can begin.
Step H must be finished before step I can begin.
Step Y must be finished before step W can begin.
Step X must be finished before step J can begin.
Step L must be finished before step W can begin.
Step G must be finished before step N can begin.
|]
