#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package "here megaparsec pretty-show split time"
-}
-- relude 
-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TupleSections #-}

-- import Relude
import Data.Char
import Data.List
-- import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.String.Here
import Data.Time
-- import Data.Time.Calendar
import Text.Show.Pretty
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow

{-

You've sneaked into another supply closet - this time, it's across
from the prototype suit manufacturing lab. You need to sneak inside
and fix the issues with the suit, but there's a guard stationed
outside the lab, so this is as close as you can safely get.

As you search the closet for anything that might help, you discover
that you're not the first person to want to sneak in. Covering the
walls, someone has spent an hour starting every midnight for the past
few months secretly observing this guard post! They've been writing
down the ID of the one guard on duty that night - the Elves seem to
have decided that one guard was enough for the overnight shift - as
well as when they fall asleep or wake up while at their post (your
puzzle input).

For example, consider the following records, which have already been
organized into chronological order:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up

Timestamps are written using year-month-day hour:minute format. The
guard falling asleep or waking up is always the one whose shift most
recently started. Because all asleep/awake times are during the
midnight hour (00:00 - 00:59), only the minute portion (00 - 59) is
relevant for those events.

Visually, these records show that the guards are asleep at these
times:

Date   ID   Minute
            000000000011111111112222222222333333333344444444445555555555
            012345678901234567890123456789012345678901234567890123456789
11-01  #10  .....####################.....#########################.....
11-02  #99  ........................................##########..........
11-03  #10  ........................#####...............................
11-04  #99  ....................................##########..............
11-05  #99  .............................................##########.....

The columns are Date, which shows the month-day portion of the
relevant day; ID, which shows the guard on duty that day; and Minute,
which shows the minutes during which the guard was asleep within the
midnight hour. (The Minute column's header shows the minute's ten's
digit in the first row and the one's digit in the second row.) Awake
is shown as ., and asleep is shown as #.

Note that guards count as asleep on the minute they fall asleep, and
they count as awake on the minute they wake up. For example, because
Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as
awake.

If you can figure out the guard most likely to be asleep at a specific
time, you might be able to trick that guard into working tonight so
you can have the best chance of sneaking in. You have two strategies
for choosing the best guard/minute combination.

Strategy 1: Find the guard that has the most minutes asleep. What
minute does that guard spend asleep the most?

In the example above, Guard #10 spent the most minutes asleep, a total
of 50 minutes (20+25+5), while Guard #99 only slept for a total of 30
minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two
days, whereas any other minute the guard was asleep was only seen on
one day).

While this example listed the entries in chronological order, your
entries are in the order you found them. You'll need to organize them
before they can be analyzed.

What is the ID of the guard you chose multiplied by the minute you
chose? (In the above example, the answer would be 10 * 24 = 240.)
-}
      
testrecords = map parserecord $ lines $ [here|
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
|]

type Id     = Int
type Time   = UTCTime
data Record =
    Guard  Id  Time
  | Sleep      Time
  | Wake       Time
  deriving (Show)
type Span   = (Time,Time)
type Minute = Int

-- parserecord :: String -> Record
-- parserecord l = r
--   where
--     Right r = parse p "" l
--     p :: Parsec String String Record
--     p = do
--       char '['
--       t <- count 16 anySingle
--       string "] "
--       choice [
--         (do
--           string "Guard #"
--           i <- read <$> some digitChar
--           string " begins shift"
--           eol
--           return $ Begin i t
--         ),
--         (string "falls asleep" >> return $ Sleep  
          
--         ]

parserecord :: String -> Record
parserecord l =
  let
    (a,b) = splitAt 19 l
    t = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" $ drop 1 $ init $ init a
  in
    case b of
      'G':'u':'a':'r':'d':' ':'#':rest -> Guard (read (takeWhile isDigit rest)) t
      'w':_ -> Wake t
      'f':_ -> Sleep t
      _ -> error $ "unexpected input: " ++ show l

type GuardsSpans = M.Map Id [Span]

guardsSpans :: [Record] -> GuardsSpans
guardsSpans rs = gs
  where
   (_,_,gs) = foldl'
    (\(mguard, msleeptime, guardsspans) r ->
      case r of
        Guard i _ -> (Just i, Nothing, guardsspans)
        Sleep t   -> (mguard, Just t,  guardsspans)
        Wake  t   -> (mguard, Nothing, guardsspans')
          where
            guardsspans' =
              M.insertWith
                (\newspan spans -> spans ++ newspan)
                (fromJust mguard)
                [(fromJust msleeptime, t)]
                guardsspans
    )
    (Nothing, Nothing, M.empty)
    (sortBy (comparing rtime) rs)

rtime (Guard _ t) = t
rtime (Sleep t) = t
rtime (Wake t) = t

testspans = guardsSpans testrecords

diffMinutes :: UTCTime -> UTCTime -> Int
diffMinutes a b = round (diffUTCTime a b) `div` 60

spanMinutes :: Span -> Int
spanMinutes (start,end) = diffMinutes end start

guardSleepMinutes :: GuardsSpans -> Id -> Int
guardSleepMinutes s i = sum $ map spanMinutes spans
  where
    spans = M.findWithDefault [] i s

first3 (x,_,_) = x
second3 (_,x,_) = x
third3 (_,_,x) = x

first4 (x,_,_,_) = x
third4 (_,_,x,_) = x
fourth4 (_,_,_,x) = x

minuteBitmap :: [Span] -> [Integer]  -- length 60
minuteBitmap spans = 
  foldl'
    (\bitmap (start,end) ->
       let
         startmin = todMin $ timeToTimeOfDay $ utctDayTime start
         (before,rest) = splitAt startmin bitmap
         (asleep,after) = splitAt (diffMinutes end start) rest
         bitmap' = before ++ map (+1) asleep ++ after
       in
         bitmap'
    )
    (replicate 60 0)
    spans

sleepiestMinutes :: [Span] -> [(Minute,Integer)]
sleepiestMinutes spans =
  reverse $ sortBy (comparing snd) $ zip [0..59] $ minuteBitmap spans

sleepiestMinute :: [Span] -> Minute
sleepiestMinute = fst . head . sleepiestMinutes

guardsStats :: GuardsSpans -> [(Id,Int,[(Minute,Integer)])] -- guard id, total minutes asleep, sleepiest minute (if there is a tie, probably the earliest)
guardsStats s =
  reverse $ sortBy (comparing second3) $
  map (\(i,spans) -> (i, sum $ map spanMinutes spans, sleepiestMinutes spans)) $ M.toList s

a = first3 s * fst (head (third3 s))
  where
    s = head $ guardsStats $ guardsSpans inputrecords
-- 63509

{-
Strategy 2: Of all guards, which guard is most frequently asleep on
the same minute?

In the example above, Guard #99 spent minute 45 asleep more than any
other guard or minute - three times in total. (In all other cases, any
guard spent any minute asleep at most twice.)

What is the ID of the guard you chose multiplied by the minute you
chose? (In the above example, the answer would be 99 * 45 = 4455.)
-}

b = gid * min
  where
    (gid,(min,_)) =
      head $ reverse $ sortBy (comparing (snd.snd)) $
      map (\s -> (first3 s, head $ third3 s)) gs
        where
          gs = guardsStats $ guardsSpans inputrecords

inputrecords = map parserecord $ lines [here|
[1518-09-29 00:35] falls asleep
[1518-09-25 00:28] falls asleep
[1518-10-14 00:00] Guard #2927 begins shift
[1518-07-20 00:53] wakes up
[1518-04-02 00:10] falls asleep
[1518-09-12 00:19] wakes up
[1518-04-18 00:35] falls asleep
[1518-04-21 00:23] wakes up
[1518-08-01 00:23] falls asleep
[1518-05-20 00:21] wakes up
[1518-03-19 00:46] wakes up
[1518-07-14 00:51] falls asleep
[1518-06-18 00:05] falls asleep
[1518-06-22 00:02] Guard #1039 begins shift
[1518-10-25 23:50] Guard #181 begins shift
[1518-04-19 00:30] wakes up
[1518-04-07 00:29] falls asleep
[1518-11-10 00:47] wakes up
[1518-04-26 00:03] Guard #1489 begins shift
[1518-05-17 00:49] wakes up
[1518-03-06 00:14] falls asleep
[1518-06-18 00:46] falls asleep
[1518-05-05 00:48] falls asleep
[1518-09-21 23:54] Guard #1553 begins shift
[1518-09-07 00:01] Guard #1489 begins shift
[1518-08-21 00:54] wakes up
[1518-11-18 00:50] falls asleep
[1518-08-21 23:50] Guard #1553 begins shift
[1518-05-05 00:25] wakes up
[1518-07-09 23:57] Guard #1597 begins shift
[1518-03-25 00:23] falls asleep
[1518-07-16 00:15] wakes up
[1518-06-19 00:02] Guard #271 begins shift
[1518-11-06 00:00] Guard #2113 begins shift
[1518-07-15 00:42] wakes up
[1518-09-06 00:39] wakes up
[1518-07-11 00:02] Guard #199 begins shift
[1518-10-23 00:00] Guard #1601 begins shift
[1518-10-06 00:44] wakes up
[1518-11-18 00:55] wakes up
[1518-04-21 00:12] falls asleep
[1518-10-18 00:57] wakes up
[1518-03-28 23:58] Guard #1553 begins shift
[1518-04-11 00:34] falls asleep
[1518-08-22 00:14] wakes up
[1518-06-14 00:58] wakes up
[1518-09-13 00:16] wakes up
[1518-03-16 00:59] wakes up
[1518-11-08 00:29] falls asleep
[1518-09-10 00:56] wakes up
[1518-03-02 23:57] Guard #2113 begins shift
[1518-07-18 00:53] falls asleep
[1518-04-24 00:41] falls asleep
[1518-09-27 00:17] falls asleep
[1518-04-15 00:50] wakes up
[1518-05-31 00:36] falls asleep
[1518-04-19 00:51] wakes up
[1518-10-29 00:01] Guard #2699 begins shift
[1518-09-23 00:14] falls asleep
[1518-03-24 00:30] wakes up
[1518-03-09 23:57] Guard #1553 begins shift
[1518-09-01 00:58] wakes up
[1518-08-02 00:32] falls asleep
[1518-10-25 00:03] falls asleep
[1518-10-31 00:38] wakes up
[1518-07-26 00:00] Guard #1553 begins shift
[1518-05-28 00:52] wakes up
[1518-11-20 00:17] falls asleep
[1518-08-31 00:37] wakes up
[1518-11-21 23:50] Guard #317 begins shift
[1518-09-21 00:58] wakes up
[1518-04-26 00:21] wakes up
[1518-08-09 00:56] wakes up
[1518-09-14 00:02] falls asleep
[1518-08-30 00:21] falls asleep
[1518-03-19 00:58] wakes up
[1518-07-28 00:04] Guard #587 begins shift
[1518-05-19 00:59] wakes up
[1518-09-05 00:40] falls asleep
[1518-08-19 00:22] falls asleep
[1518-06-28 00:07] falls asleep
[1518-05-29 00:17] falls asleep
[1518-08-12 00:53] wakes up
[1518-05-28 00:51] falls asleep
[1518-11-06 00:51] falls asleep
[1518-10-12 23:50] Guard #2699 begins shift
[1518-07-16 00:23] falls asleep
[1518-09-22 00:50] wakes up
[1518-06-29 00:25] falls asleep
[1518-06-02 23:58] Guard #1601 begins shift
[1518-08-07 00:15] falls asleep
[1518-10-25 00:52] wakes up
[1518-05-04 00:52] falls asleep
[1518-11-04 23:56] Guard #1549 begins shift
[1518-11-22 00:04] falls asleep
[1518-04-21 23:59] Guard #1433 begins shift
[1518-07-10 00:32] wakes up
[1518-08-16 00:43] wakes up
[1518-05-23 00:13] falls asleep
[1518-08-19 00:30] falls asleep
[1518-06-04 00:59] wakes up
[1518-06-07 00:21] wakes up
[1518-09-13 23:53] Guard #587 begins shift
[1518-09-13 00:58] wakes up
[1518-11-19 23:57] Guard #1433 begins shift
[1518-09-20 00:22] falls asleep
[1518-03-16 00:55] falls asleep
[1518-08-19 00:26] wakes up
[1518-06-17 00:34] wakes up
[1518-06-01 00:59] wakes up
[1518-08-29 23:57] Guard #1601 begins shift
[1518-05-12 00:30] wakes up
[1518-03-02 00:41] falls asleep
[1518-10-04 00:03] Guard #587 begins shift
[1518-03-19 00:04] Guard #1553 begins shift
[1518-10-09 23:57] Guard #3121 begins shift
[1518-09-30 23:57] Guard #587 begins shift
[1518-08-16 00:36] falls asleep
[1518-10-20 00:10] falls asleep
[1518-04-25 00:56] falls asleep
[1518-08-20 00:16] falls asleep
[1518-08-13 00:04] Guard #239 begins shift
[1518-05-20 00:03] Guard #1543 begins shift
[1518-04-30 00:16] falls asleep
[1518-08-05 00:59] wakes up
[1518-09-19 00:48] wakes up
[1518-11-10 00:09] falls asleep
[1518-04-27 00:03] Guard #2699 begins shift
[1518-03-03 00:54] falls asleep
[1518-03-19 00:35] falls asleep
[1518-07-28 00:59] wakes up
[1518-10-20 00:53] wakes up
[1518-08-13 00:51] wakes up
[1518-07-09 00:50] wakes up
[1518-10-22 00:41] falls asleep
[1518-06-09 00:58] wakes up
[1518-10-16 23:57] Guard #181 begins shift
[1518-05-27 00:05] falls asleep
[1518-09-27 00:00] Guard #1553 begins shift
[1518-09-03 00:42] falls asleep
[1518-06-28 23:58] Guard #1549 begins shift
[1518-08-08 00:56] wakes up
[1518-11-03 00:28] falls asleep
[1518-09-24 00:45] wakes up
[1518-10-16 00:57] wakes up
[1518-11-19 00:50] falls asleep
[1518-10-11 00:40] wakes up
[1518-06-13 23:58] Guard #1489 begins shift
[1518-09-13 00:03] falls asleep
[1518-09-23 00:33] wakes up
[1518-04-28 23:56] Guard #2113 begins shift
[1518-11-11 00:56] wakes up
[1518-06-07 00:26] falls asleep
[1518-03-31 00:37] wakes up
[1518-03-30 00:52] wakes up
[1518-09-28 00:03] falls asleep
[1518-03-21 00:00] falls asleep
[1518-10-27 23:48] Guard #229 begins shift
[1518-06-23 00:06] falls asleep
[1518-09-17 23:54] Guard #1597 begins shift
[1518-11-23 00:12] falls asleep
[1518-05-02 00:24] falls asleep
[1518-06-25 00:38] falls asleep
[1518-07-04 00:31] wakes up
[1518-11-01 23:47] Guard #2113 begins shift
[1518-06-30 00:48] wakes up
[1518-10-31 23:56] Guard #2699 begins shift
[1518-07-09 00:47] falls asleep
[1518-11-08 00:22] wakes up
[1518-09-13 00:29] falls asleep
[1518-03-03 00:56] wakes up
[1518-04-12 00:27] falls asleep
[1518-10-10 23:59] Guard #181 begins shift
[1518-11-13 00:59] wakes up
[1518-05-31 00:32] wakes up
[1518-08-02 00:40] wakes up
[1518-04-22 00:53] falls asleep
[1518-11-01 00:49] wakes up
[1518-06-06 00:02] Guard #229 begins shift
[1518-05-08 00:51] wakes up
[1518-06-14 23:56] Guard #1433 begins shift
[1518-10-19 00:24] wakes up
[1518-04-02 00:30] falls asleep
[1518-08-14 00:07] falls asleep
[1518-04-01 00:00] Guard #1423 begins shift
[1518-10-27 00:19] wakes up
[1518-05-04 23:50] Guard #199 begins shift
[1518-07-07 00:58] wakes up
[1518-09-12 00:13] falls asleep
[1518-09-27 00:27] wakes up
[1518-08-23 00:31] wakes up
[1518-07-07 00:04] Guard #1433 begins shift
[1518-06-10 00:14] falls asleep
[1518-04-03 00:48] wakes up
[1518-10-18 00:01] Guard #317 begins shift
[1518-10-01 00:35] falls asleep
[1518-07-31 00:14] falls asleep
[1518-03-06 23:54] Guard #2039 begins shift
[1518-04-08 00:44] wakes up
[1518-05-14 00:58] wakes up
[1518-04-12 00:04] Guard #1433 begins shift
[1518-06-22 00:36] wakes up
[1518-11-02 00:05] falls asleep
[1518-09-01 00:14] falls asleep
[1518-11-16 00:24] falls asleep
[1518-03-04 00:13] falls asleep
[1518-10-24 00:48] falls asleep
[1518-06-12 00:30] wakes up
[1518-04-29 00:58] wakes up
[1518-07-21 00:06] falls asleep
[1518-03-11 00:55] wakes up
[1518-11-04 00:38] falls asleep
[1518-09-26 00:03] falls asleep
[1518-10-31 00:24] wakes up
[1518-03-09 00:18] wakes up
[1518-06-17 00:49] falls asleep
[1518-09-20 00:42] falls asleep
[1518-10-02 00:23] wakes up
[1518-04-06 00:43] falls asleep
[1518-07-23 00:56] falls asleep
[1518-08-18 00:45] falls asleep
[1518-05-24 00:00] Guard #199 begins shift
[1518-07-26 00:50] wakes up
[1518-05-17 00:26] falls asleep
[1518-06-10 00:46] wakes up
[1518-03-06 00:43] falls asleep
[1518-08-27 00:11] falls asleep
[1518-10-18 00:48] wakes up
[1518-05-19 00:50] falls asleep
[1518-03-13 00:06] falls asleep
[1518-03-04 00:40] falls asleep
[1518-10-06 00:48] falls asleep
[1518-11-05 00:54] falls asleep
[1518-07-23 00:00] Guard #1543 begins shift
[1518-06-17 00:50] wakes up
[1518-05-23 00:30] wakes up
[1518-10-29 00:59] wakes up
[1518-08-29 00:57] wakes up
[1518-06-14 00:28] wakes up
[1518-07-08 00:53] falls asleep
[1518-03-02 00:45] wakes up
[1518-10-15 23:57] Guard #1549 begins shift
[1518-08-01 00:58] wakes up
[1518-09-05 00:58] wakes up
[1518-10-23 00:29] falls asleep
[1518-08-29 00:23] falls asleep
[1518-06-04 00:00] Guard #1489 begins shift
[1518-04-04 00:32] wakes up
[1518-06-01 23:48] Guard #1549 begins shift
[1518-08-31 00:49] falls asleep
[1518-05-08 00:31] falls asleep
[1518-03-11 00:01] Guard #1433 begins shift
[1518-04-24 00:56] falls asleep
[1518-08-27 00:56] wakes up
[1518-10-18 23:58] Guard #1549 begins shift
[1518-03-15 00:58] wakes up
[1518-10-29 00:53] falls asleep
[1518-03-28 00:50] falls asleep
[1518-05-15 00:45] falls asleep
[1518-04-24 23:57] Guard #1549 begins shift
[1518-05-28 00:00] Guard #181 begins shift
[1518-04-16 00:35] falls asleep
[1518-10-21 00:01] Guard #1489 begins shift
[1518-06-20 00:00] Guard #199 begins shift
[1518-05-21 00:00] Guard #2039 begins shift
[1518-06-22 00:18] falls asleep
[1518-09-24 00:02] Guard #2927 begins shift
[1518-11-07 00:02] Guard #593 begins shift
[1518-04-28 00:57] wakes up
[1518-04-14 00:44] wakes up
[1518-07-14 00:57] falls asleep
[1518-06-21 00:51] wakes up
[1518-03-04 00:37] wakes up
[1518-10-07 00:44] falls asleep
[1518-07-23 23:57] Guard #1433 begins shift
[1518-09-25 00:02] Guard #1597 begins shift
[1518-07-04 00:26] falls asleep
[1518-04-30 23:59] Guard #1433 begins shift
[1518-08-28 00:44] wakes up
[1518-10-21 00:49] wakes up
[1518-09-13 00:54] falls asleep
[1518-06-24 00:52] wakes up
[1518-11-06 00:58] wakes up
[1518-03-14 00:53] wakes up
[1518-06-23 00:52] wakes up
[1518-08-15 23:58] Guard #199 begins shift
[1518-05-22 00:03] Guard #1553 begins shift
[1518-07-14 00:58] wakes up
[1518-05-02 00:35] falls asleep
[1518-07-08 00:56] wakes up
[1518-07-06 00:04] falls asleep
[1518-03-03 23:58] Guard #2699 begins shift
[1518-03-01 23:47] Guard #587 begins shift
[1518-03-14 00:07] falls asleep
[1518-03-12 00:55] falls asleep
[1518-03-06 00:16] wakes up
[1518-05-09 23:57] Guard #1549 begins shift
[1518-08-13 00:56] falls asleep
[1518-06-06 00:55] wakes up
[1518-05-28 00:27] falls asleep
[1518-04-30 00:12] wakes up
[1518-07-23 00:14] falls asleep
[1518-03-08 00:03] Guard #587 begins shift
[1518-06-30 00:16] wakes up
[1518-09-07 00:58] wakes up
[1518-08-18 00:56] falls asleep
[1518-03-12 00:56] wakes up
[1518-05-21 00:32] wakes up
[1518-10-08 00:59] wakes up
[1518-06-06 23:58] Guard #181 begins shift
[1518-03-21 00:59] wakes up
[1518-08-24 00:19] wakes up
[1518-03-19 00:41] wakes up
[1518-07-02 00:49] wakes up
[1518-03-07 00:39] falls asleep
[1518-04-29 00:09] falls asleep
[1518-10-26 00:47] wakes up
[1518-03-22 00:01] Guard #1601 begins shift
[1518-08-03 00:50] wakes up
[1518-09-09 23:58] Guard #1549 begins shift
[1518-04-20 00:59] wakes up
[1518-05-07 00:27] wakes up
[1518-08-03 23:58] Guard #3121 begins shift
[1518-06-16 00:13] falls asleep
[1518-03-12 00:52] wakes up
[1518-08-07 00:31] wakes up
[1518-03-12 00:10] falls asleep
[1518-10-19 00:06] falls asleep
[1518-03-17 00:57] wakes up
[1518-09-12 23:49] Guard #1553 begins shift
[1518-07-18 23:57] Guard #587 begins shift
[1518-10-04 00:55] wakes up
[1518-09-26 00:33] wakes up
[1518-04-06 00:40] wakes up
[1518-05-31 00:02] Guard #1489 begins shift
[1518-03-16 00:02] Guard #1553 begins shift
[1518-04-04 00:24] falls asleep
[1518-03-19 00:50] falls asleep
[1518-04-18 00:50] falls asleep
[1518-05-14 00:00] Guard #181 begins shift
[1518-10-03 00:03] Guard #1549 begins shift
[1518-09-08 00:28] falls asleep
[1518-11-21 00:12] falls asleep
[1518-07-11 23:46] Guard #1433 begins shift
[1518-09-16 00:55] wakes up
[1518-10-18 00:39] falls asleep
[1518-07-03 00:21] falls asleep
[1518-08-09 00:44] falls asleep
[1518-10-27 00:31] falls asleep
[1518-06-30 00:00] falls asleep
[1518-10-05 23:59] Guard #1601 begins shift
[1518-04-09 00:53] falls asleep
[1518-08-10 00:04] Guard #1423 begins shift
[1518-07-23 00:43] wakes up
[1518-06-27 00:00] Guard #1601 begins shift
[1518-08-07 00:00] Guard #2113 begins shift
[1518-07-10 00:53] wakes up
[1518-08-14 00:49] wakes up
[1518-06-09 00:29] falls asleep
[1518-03-18 00:29] wakes up
[1518-06-17 23:53] Guard #239 begins shift
[1518-10-03 00:44] wakes up
[1518-03-20 00:22] falls asleep
[1518-04-25 00:52] wakes up
[1518-05-07 00:54] falls asleep
[1518-04-04 00:04] Guard #1489 begins shift
[1518-06-28 00:35] falls asleep
[1518-04-17 00:44] wakes up
[1518-09-23 00:01] Guard #2039 begins shift
[1518-06-07 23:57] Guard #1423 begins shift
[1518-04-19 00:15] wakes up
[1518-07-20 00:32] falls asleep
[1518-03-29 00:21] falls asleep
[1518-07-08 00:06] falls asleep
[1518-10-03 00:49] falls asleep
[1518-05-13 00:42] wakes up
[1518-11-12 00:34] falls asleep
[1518-04-18 00:47] wakes up
[1518-07-13 00:16] falls asleep
[1518-10-29 00:46] wakes up
[1518-09-22 00:00] falls asleep
[1518-09-18 00:24] wakes up
[1518-09-18 00:05] falls asleep
[1518-05-20 00:56] falls asleep
[1518-07-14 00:31] wakes up
[1518-08-20 23:58] Guard #587 begins shift
[1518-11-14 00:06] falls asleep
[1518-05-16 00:03] Guard #1543 begins shift
[1518-04-18 23:58] Guard #2113 begins shift
[1518-04-26 00:48] wakes up
[1518-03-09 00:11] falls asleep
[1518-06-29 00:47] wakes up
[1518-09-17 00:00] Guard #1489 begins shift
[1518-03-24 00:55] wakes up
[1518-11-08 00:00] Guard #2113 begins shift
[1518-03-27 23:59] Guard #1549 begins shift
[1518-08-07 00:56] wakes up
[1518-07-17 23:47] Guard #2113 begins shift
[1518-10-29 00:21] falls asleep
[1518-10-01 00:47] falls asleep
[1518-09-21 00:20] falls asleep
[1518-07-07 00:28] wakes up
[1518-11-01 00:59] wakes up
[1518-03-04 00:45] wakes up
[1518-03-11 00:36] wakes up
[1518-10-30 00:44] falls asleep
[1518-08-02 00:58] wakes up
[1518-07-08 00:33] wakes up
[1518-08-09 00:02] falls asleep
[1518-04-30 00:03] falls asleep
[1518-11-08 00:51] wakes up
[1518-07-12 00:26] wakes up
[1518-07-12 00:52] wakes up
[1518-09-02 23:48] Guard #2113 begins shift
[1518-06-10 00:49] falls asleep
[1518-07-29 00:12] wakes up
[1518-06-18 00:50] wakes up
[1518-06-06 00:43] falls asleep
[1518-11-20 00:53] wakes up
[1518-11-09 00:26] falls asleep
[1518-07-31 00:23] wakes up
[1518-04-24 00:47] wakes up
[1518-08-17 00:47] wakes up
[1518-03-16 23:57] Guard #587 begins shift
[1518-06-14 00:26] falls asleep
[1518-06-09 23:56] Guard #1543 begins shift
[1518-03-24 00:00] Guard #1553 begins shift
[1518-04-28 00:44] falls asleep
[1518-05-15 00:27] falls asleep
[1518-04-23 00:15] falls asleep
[1518-03-12 00:30] falls asleep
[1518-10-24 00:03] falls asleep
[1518-07-13 00:00] Guard #3121 begins shift
[1518-07-30 00:00] Guard #1601 begins shift
[1518-11-10 00:00] Guard #2039 begins shift
[1518-09-08 00:04] Guard #229 begins shift
[1518-05-09 00:03] Guard #229 begins shift
[1518-10-09 00:15] falls asleep
[1518-07-12 00:59] wakes up
[1518-07-31 23:59] Guard #1597 begins shift
[1518-06-15 00:57] wakes up
[1518-05-11 00:01] Guard #1549 begins shift
[1518-08-25 00:25] falls asleep
[1518-07-18 00:46] wakes up
[1518-05-22 00:57] wakes up
[1518-08-28 00:31] falls asleep
[1518-10-02 00:01] Guard #2039 begins shift
[1518-08-25 00:49] falls asleep
[1518-04-05 23:57] Guard #1549 begins shift
[1518-10-08 00:45] wakes up
[1518-04-26 00:35] falls asleep
[1518-03-07 00:04] falls asleep
[1518-06-16 00:00] Guard #1597 begins shift
[1518-03-24 00:46] wakes up
[1518-09-14 00:56] wakes up
[1518-08-24 00:36] wakes up
[1518-09-15 23:52] Guard #3121 begins shift
[1518-11-15 00:26] falls asleep
[1518-05-28 00:07] falls asleep
[1518-10-03 00:41] falls asleep
[1518-11-04 00:58] wakes up
[1518-10-14 00:40] falls asleep
[1518-07-30 00:06] falls asleep
[1518-05-03 00:27] falls asleep
[1518-08-08 00:47] falls asleep
[1518-08-30 00:48] wakes up
[1518-08-18 00:46] wakes up
[1518-06-03 00:33] falls asleep
[1518-05-22 00:56] falls asleep
[1518-03-25 00:55] wakes up
[1518-07-17 00:34] falls asleep
[1518-04-03 00:04] Guard #2699 begins shift
[1518-11-22 23:57] Guard #1489 begins shift
[1518-08-19 00:34] wakes up
[1518-11-01 00:47] falls asleep
[1518-05-05 00:58] wakes up
[1518-03-05 23:58] Guard #1543 begins shift
[1518-10-04 00:27] falls asleep
[1518-06-13 00:55] wakes up
[1518-05-26 00:08] falls asleep
[1518-05-24 00:50] falls asleep
[1518-10-28 00:04] falls asleep
[1518-11-16 23:53] Guard #1601 begins shift
[1518-05-14 00:30] wakes up
[1518-08-21 00:34] wakes up
[1518-07-10 00:46] falls asleep
[1518-08-27 00:49] falls asleep
[1518-05-25 23:58] Guard #1597 begins shift
[1518-08-31 00:18] falls asleep
[1518-08-12 00:23] wakes up
[1518-05-19 00:15] falls asleep
[1518-05-30 00:45] wakes up
[1518-06-11 00:47] wakes up
[1518-09-28 00:25] wakes up
[1518-08-20 00:00] Guard #2039 begins shift
[1518-10-27 00:33] wakes up
[1518-06-17 00:02] Guard #1597 begins shift
[1518-08-09 00:29] wakes up
[1518-07-04 23:56] Guard #1489 begins shift
[1518-03-27 00:48] falls asleep
[1518-06-12 00:43] falls asleep
[1518-10-20 00:04] Guard #1489 begins shift
[1518-06-05 00:54] wakes up
[1518-06-25 23:57] Guard #1597 begins shift
[1518-10-26 23:50] Guard #1543 begins shift
[1518-11-08 00:19] falls asleep
[1518-10-02 00:32] wakes up
[1518-09-19 00:02] Guard #1597 begins shift
[1518-04-12 00:52] wakes up
[1518-03-14 00:00] Guard #1549 begins shift
[1518-09-09 00:26] falls asleep
[1518-03-25 23:59] Guard #1553 begins shift
[1518-07-22 00:47] falls asleep
[1518-04-20 00:41] falls asleep
[1518-11-11 00:11] falls asleep
[1518-10-17 00:23] falls asleep
[1518-11-21 00:26] wakes up
[1518-10-07 00:32] wakes up
[1518-07-07 00:44] wakes up
[1518-11-12 00:53] wakes up
[1518-06-27 00:45] wakes up
[1518-05-11 00:56] wakes up
[1518-08-05 23:59] Guard #1039 begins shift
[1518-03-09 00:54] wakes up
[1518-09-13 00:50] wakes up
[1518-07-05 00:44] falls asleep
[1518-06-11 23:58] Guard #1553 begins shift
[1518-03-31 00:32] falls asleep
[1518-10-28 00:47] falls asleep
[1518-04-08 00:01] falls asleep
[1518-11-03 00:00] Guard #587 begins shift
[1518-08-08 23:46] Guard #2113 begins shift
[1518-07-11 00:59] wakes up
[1518-05-26 23:46] Guard #2113 begins shift
[1518-08-19 00:00] Guard #3121 begins shift
[1518-05-31 00:08] falls asleep
[1518-03-15 00:33] wakes up
[1518-06-15 00:34] wakes up
[1518-03-13 00:37] falls asleep
[1518-04-10 00:35] wakes up
[1518-10-02 00:22] falls asleep
[1518-04-28 00:07] wakes up
[1518-09-09 00:56] falls asleep
[1518-05-20 00:59] wakes up
[1518-09-06 00:00] Guard #1039 begins shift
[1518-07-16 00:10] falls asleep
[1518-07-16 00:26] wakes up
[1518-06-18 00:26] wakes up
[1518-03-24 00:53] falls asleep
[1518-07-24 00:41] falls asleep
[1518-05-20 00:06] falls asleep
[1518-03-16 00:48] wakes up
[1518-06-26 00:33] wakes up
[1518-09-02 00:54] wakes up
[1518-08-15 00:35] falls asleep
[1518-09-30 00:01] Guard #1549 begins shift
[1518-03-20 23:50] Guard #1553 begins shift
[1518-07-07 00:07] falls asleep
[1518-11-09 00:49] wakes up
[1518-03-26 00:48] falls asleep
[1518-05-29 00:57] wakes up
[1518-06-20 00:50] falls asleep
[1518-03-30 00:46] falls asleep
[1518-11-18 00:20] falls asleep
[1518-06-17 00:29] falls asleep
[1518-09-11 00:00] Guard #3121 begins shift
[1518-05-08 00:04] Guard #1489 begins shift
[1518-10-28 00:59] wakes up
[1518-09-19 00:26] falls asleep
[1518-08-23 23:47] Guard #1597 begins shift
[1518-06-12 00:10] falls asleep
[1518-08-27 00:41] wakes up
[1518-07-23 00:58] wakes up
[1518-08-22 00:00] falls asleep
[1518-06-23 00:04] Guard #1549 begins shift
[1518-11-19 00:01] falls asleep
[1518-11-14 00:04] Guard #239 begins shift
[1518-04-07 00:00] Guard #1433 begins shift
[1518-05-15 00:30] wakes up
[1518-06-25 00:54] wakes up
[1518-05-30 00:38] falls asleep
[1518-09-25 00:45] wakes up
[1518-03-02 00:36] wakes up
[1518-11-15 00:57] wakes up
[1518-04-18 00:57] falls asleep
[1518-05-12 00:57] wakes up
[1518-06-10 00:54] wakes up
[1518-03-15 00:29] falls asleep
[1518-09-30 00:25] falls asleep
[1518-10-07 00:04] Guard #3121 begins shift
[1518-04-20 00:24] wakes up
[1518-04-28 00:04] falls asleep
[1518-04-09 00:55] wakes up
[1518-09-23 00:48] falls asleep
[1518-11-21 00:03] Guard #2039 begins shift
[1518-07-12 00:03] falls asleep
[1518-10-06 00:52] wakes up
[1518-05-26 00:12] wakes up
[1518-06-06 00:54] falls asleep
[1518-05-16 00:56] wakes up
[1518-04-10 00:20] falls asleep
[1518-07-11 00:54] wakes up
[1518-08-22 00:31] falls asleep
[1518-08-26 00:01] Guard #1597 begins shift
[1518-05-04 00:57] wakes up
[1518-07-02 00:06] falls asleep
[1518-06-08 23:59] Guard #587 begins shift
[1518-10-21 00:37] wakes up
[1518-05-17 00:57] wakes up
[1518-09-10 00:23] falls asleep
[1518-11-10 23:59] Guard #1489 begins shift
[1518-08-23 00:03] Guard #2039 begins shift
[1518-09-06 00:29] falls asleep
[1518-11-17 23:57] Guard #317 begins shift
[1518-07-31 00:02] Guard #1549 begins shift
[1518-08-18 00:41] wakes up
[1518-05-09 00:55] wakes up
[1518-07-13 00:36] falls asleep
[1518-03-09 00:32] falls asleep
[1518-05-09 00:40] falls asleep
[1518-10-30 00:58] wakes up
[1518-08-27 23:57] Guard #587 begins shift
[1518-10-31 00:19] falls asleep
[1518-09-05 00:01] Guard #1433 begins shift
[1518-03-07 00:36] wakes up
[1518-06-25 00:00] Guard #2699 begins shift
[1518-11-07 00:37] falls asleep
[1518-06-29 23:49] Guard #2927 begins shift
[1518-09-08 23:58] Guard #1601 begins shift
[1518-03-08 00:51] wakes up
[1518-07-05 00:40] wakes up
[1518-08-04 00:25] falls asleep
[1518-09-29 00:04] Guard #1039 begins shift
[1518-06-26 00:20] falls asleep
[1518-05-10 00:57] wakes up
[1518-10-07 00:52] wakes up
[1518-04-24 00:35] wakes up
[1518-05-19 00:01] Guard #1601 begins shift
[1518-09-17 00:07] falls asleep
[1518-08-18 00:02] falls asleep
[1518-10-18 00:33] wakes up
[1518-05-01 00:51] falls asleep
[1518-05-12 00:36] falls asleep
[1518-03-07 00:52] wakes up
[1518-03-22 00:58] wakes up
[1518-08-06 00:35] wakes up
[1518-04-25 00:40] falls asleep
[1518-11-15 00:30] wakes up
[1518-06-06 00:48] wakes up
[1518-03-05 00:14] falls asleep
[1518-10-10 00:31] falls asleep
[1518-05-27 00:38] wakes up
[1518-11-01 00:56] falls asleep
[1518-06-27 00:27] falls asleep
[1518-05-10 00:27] falls asleep
[1518-09-01 00:02] Guard #2699 begins shift
[1518-07-29 00:17] falls asleep
[1518-03-24 00:16] falls asleep
[1518-04-29 23:54] Guard #1597 begins shift
[1518-10-18 00:56] falls asleep
[1518-03-13 00:08] wakes up
[1518-06-02 00:52] wakes up
[1518-05-02 23:58] Guard #1039 begins shift
[1518-07-25 00:02] Guard #1423 begins shift
[1518-09-04 00:00] Guard #1553 begins shift
[1518-07-22 00:55] wakes up
[1518-06-24 00:00] Guard #199 begins shift
[1518-04-02 00:22] wakes up
[1518-08-27 00:00] Guard #1489 begins shift
[1518-11-19 00:09] wakes up
[1518-03-19 00:44] falls asleep
[1518-07-08 00:42] wakes up
[1518-05-13 00:03] Guard #2039 begins shift
[1518-06-12 23:52] Guard #1433 begins shift
[1518-04-13 00:25] falls asleep
[1518-11-21 00:57] wakes up
[1518-09-11 23:59] Guard #2699 begins shift
[1518-06-21 00:56] falls asleep
[1518-05-31 00:50] wakes up
[1518-05-12 00:27] falls asleep
[1518-07-27 00:32] falls asleep
[1518-03-24 23:56] Guard #1549 begins shift
[1518-07-07 00:35] falls asleep
[1518-09-03 00:00] falls asleep
[1518-07-10 00:27] falls asleep
[1518-11-09 00:04] Guard #317 begins shift
[1518-06-28 00:04] Guard #587 begins shift
[1518-05-30 00:00] Guard #2927 begins shift
[1518-09-04 00:18] falls asleep
[1518-06-15 00:38] falls asleep
[1518-03-12 00:22] wakes up
[1518-06-05 00:39] falls asleep
[1518-11-07 00:38] wakes up
[1518-04-13 00:55] wakes up
[1518-08-25 00:56] wakes up
[1518-07-11 00:06] falls asleep
[1518-07-15 00:06] falls asleep
[1518-05-22 00:53] wakes up
[1518-05-18 00:37] wakes up
[1518-03-17 00:23] falls asleep
[1518-08-13 23:58] Guard #2039 begins shift
[1518-11-23 00:43] wakes up
[1518-08-03 00:15] falls asleep
[1518-10-22 00:57] wakes up
[1518-07-05 00:27] falls asleep
[1518-11-21 00:48] falls asleep
[1518-10-28 00:27] wakes up
[1518-11-13 00:33] falls asleep
[1518-07-08 23:59] Guard #239 begins shift
[1518-05-15 00:00] Guard #587 begins shift
[1518-07-14 00:53] wakes up
[1518-09-11 00:08] falls asleep
[1518-05-15 00:48] wakes up
[1518-04-16 00:52] wakes up
[1518-10-30 23:57] Guard #1597 begins shift
[1518-05-28 00:47] wakes up
[1518-05-30 00:16] falls asleep
[1518-03-23 00:02] Guard #229 begins shift
[1518-03-13 00:00] Guard #3121 begins shift
[1518-08-20 00:49] falls asleep
[1518-10-13 00:46] wakes up
[1518-10-11 00:19] falls asleep
[1518-07-26 00:27] falls asleep
[1518-11-17 00:47] wakes up
[1518-04-14 23:58] Guard #587 begins shift
[1518-11-06 00:34] wakes up
[1518-09-03 00:21] wakes up
[1518-03-25 00:50] wakes up
[1518-04-02 00:38] wakes up
[1518-08-10 23:59] Guard #2377 begins shift
[1518-06-15 00:15] falls asleep
[1518-10-08 00:42] falls asleep
[1518-05-20 00:42] wakes up
[1518-06-13 00:38] wakes up
[1518-10-30 00:02] Guard #239 begins shift
[1518-07-02 00:17] wakes up
[1518-08-18 00:59] wakes up
[1518-04-10 00:00] Guard #587 begins shift
[1518-10-24 23:50] Guard #1553 begins shift
[1518-07-05 00:16] falls asleep
[1518-10-12 00:54] falls asleep
[1518-05-23 00:00] Guard #587 begins shift
[1518-06-04 23:56] Guard #2699 begins shift
[1518-06-20 00:13] falls asleep
[1518-07-05 00:47] wakes up
[1518-07-24 00:54] wakes up
[1518-04-24 00:57] wakes up
[1518-10-31 00:29] falls asleep
[1518-10-11 23:57] Guard #1601 begins shift
[1518-06-21 00:01] Guard #2699 begins shift
[1518-10-27 00:41] falls asleep
[1518-06-25 00:34] wakes up
[1518-08-09 00:50] wakes up
[1518-07-01 00:31] falls asleep
[1518-10-14 23:57] Guard #2377 begins shift
[1518-03-11 00:40] falls asleep
[1518-03-28 00:57] wakes up
[1518-07-18 00:00] falls asleep
[1518-05-20 00:36] falls asleep
[1518-09-25 00:33] wakes up
[1518-10-13 00:05] falls asleep
[1518-08-21 00:38] falls asleep
[1518-08-13 00:46] falls asleep
[1518-05-07 00:00] Guard #3121 begins shift
[1518-09-02 00:23] falls asleep
[1518-10-09 00:22] wakes up
[1518-07-28 00:14] falls asleep
[1518-11-01 00:35] falls asleep
[1518-03-23 00:44] falls asleep
[1518-07-12 00:55] falls asleep
[1518-10-23 00:39] wakes up
[1518-07-12 00:39] falls asleep
[1518-07-08 00:37] falls asleep
[1518-11-21 00:33] falls asleep
[1518-11-05 00:55] wakes up
[1518-07-27 00:37] wakes up
[1518-07-07 23:58] Guard #593 begins shift
[1518-09-11 00:52] wakes up
[1518-03-04 23:58] Guard #593 begins shift
[1518-04-07 23:47] Guard #1601 begins shift
[1518-04-14 00:09] falls asleep
[1518-09-14 00:47] falls asleep
[1518-09-12 00:32] falls asleep
[1518-11-21 00:42] wakes up
[1518-07-03 00:01] Guard #1489 begins shift
[1518-05-25 00:55] wakes up
[1518-11-22 00:38] wakes up
[1518-05-26 00:48] wakes up
[1518-04-30 00:37] wakes up
[1518-06-03 00:46] wakes up
[1518-03-16 00:41] falls asleep
[1518-07-21 00:53] wakes up
[1518-09-30 00:50] wakes up
[1518-08-11 23:50] Guard #1601 begins shift
[1518-10-21 00:13] falls asleep
[1518-04-24 00:00] Guard #1597 begins shift
[1518-08-28 23:56] Guard #3121 begins shift
[1518-08-15 00:58] wakes up
[1518-07-28 23:48] Guard #593 begins shift
[1518-08-04 00:46] wakes up
[1518-11-04 00:03] Guard #1039 begins shift
[1518-11-14 00:51] wakes up
[1518-07-13 00:23] wakes up
[1518-08-26 00:36] wakes up
[1518-03-10 00:46] wakes up
[1518-04-10 23:59] Guard #587 begins shift
[1518-06-30 00:40] falls asleep
[1518-05-06 00:52] wakes up
[1518-04-19 23:50] Guard #1601 begins shift
[1518-08-22 00:37] wakes up
[1518-08-12 00:04] falls asleep
[1518-10-01 00:52] wakes up
[1518-04-18 00:53] wakes up
[1518-09-17 00:57] wakes up
[1518-05-03 23:57] Guard #3121 begins shift
[1518-09-09 00:43] wakes up
[1518-07-14 00:02] Guard #229 begins shift
[1518-10-24 00:43] wakes up
[1518-05-06 00:47] falls asleep
[1518-09-16 00:04] falls asleep
[1518-09-16 00:07] wakes up
[1518-05-02 00:38] wakes up
[1518-07-17 00:53] wakes up
[1518-04-03 00:45] falls asleep
[1518-04-19 00:47] falls asleep
[1518-05-17 00:52] falls asleep
[1518-03-05 00:32] wakes up
[1518-08-02 00:49] falls asleep
[1518-07-16 00:03] Guard #1489 begins shift
[1518-10-03 00:58] wakes up
[1518-07-11 00:07] wakes up
[1518-06-28 00:50] wakes up
[1518-04-27 23:50] Guard #2699 begins shift
[1518-10-07 23:50] Guard #593 begins shift
[1518-04-26 00:14] falls asleep
[1518-03-29 00:48] wakes up
[1518-04-05 00:22] wakes up
[1518-05-07 00:13] falls asleep
[1518-05-06 00:00] Guard #229 begins shift
[1518-04-22 00:55] wakes up
[1518-03-30 23:57] Guard #593 begins shift
[1518-08-06 00:23] falls asleep
[1518-03-21 00:41] wakes up
[1518-05-14 00:52] falls asleep
[1518-10-25 00:49] falls asleep
[1518-09-03 00:54] wakes up
[1518-04-15 00:22] falls asleep
[1518-05-01 00:54] wakes up
[1518-10-23 23:54] Guard #1549 begins shift
[1518-03-17 23:49] Guard #1601 begins shift
[1518-05-25 00:53] falls asleep
[1518-11-15 00:49] falls asleep
[1518-09-25 00:36] falls asleep
[1518-06-21 00:48] falls asleep
[1518-10-22 00:03] Guard #2699 begins shift
[1518-08-05 00:01] Guard #181 begins shift
[1518-11-19 00:53] wakes up
[1518-08-09 00:55] falls asleep
[1518-06-02 00:00] falls asleep
[1518-06-16 00:41] wakes up
[1518-11-11 23:59] Guard #2699 begins shift
[1518-08-23 00:49] wakes up
[1518-09-08 00:51] wakes up
[1518-10-08 00:04] falls asleep
[1518-04-11 00:45] wakes up
[1518-11-16 00:35] wakes up
[1518-10-08 00:57] falls asleep
[1518-11-23 00:53] falls asleep
[1518-07-19 00:21] falls asleep
[1518-04-19 00:13] falls asleep
[1518-10-27 00:04] falls asleep
[1518-08-07 00:47] falls asleep
[1518-05-02 00:03] Guard #3121 begins shift
[1518-08-20 00:54] wakes up
[1518-10-09 00:04] Guard #181 begins shift
[1518-03-15 00:00] Guard #587 begins shift
[1518-06-12 00:50] wakes up
[1518-08-08 00:02] Guard #2699 begins shift
[1518-03-21 00:45] falls asleep
[1518-03-18 00:00] falls asleep
[1518-08-13 00:58] wakes up
[1518-09-01 23:57] Guard #1597 begins shift
[1518-09-07 00:22] falls asleep
[1518-11-13 00:03] Guard #1597 begins shift
[1518-08-25 00:45] wakes up
[1518-09-12 00:52] wakes up
[1518-06-30 23:56] Guard #2927 begins shift
[1518-08-24 00:04] falls asleep
[1518-09-20 00:59] wakes up
[1518-05-26 00:26] falls asleep
[1518-05-21 00:14] falls asleep
[1518-10-21 00:58] wakes up
[1518-08-20 00:35] wakes up
[1518-04-09 00:01] Guard #199 begins shift
[1518-08-24 00:22] falls asleep
[1518-03-06 00:50] wakes up
[1518-09-27 23:54] Guard #593 begins shift
[1518-05-16 00:44] falls asleep
[1518-04-12 23:56] Guard #1549 begins shift
[1518-05-05 00:00] falls asleep
[1518-10-08 00:25] wakes up
[1518-04-27 00:47] falls asleep
[1518-09-19 23:57] Guard #1601 begins shift
[1518-06-24 00:19] falls asleep
[1518-07-16 00:41] wakes up
[1518-08-12 00:29] falls asleep
[1518-07-29 00:04] falls asleep
[1518-08-29 00:52] wakes up
[1518-05-13 00:29] falls asleep
[1518-07-02 00:32] falls asleep
[1518-08-30 23:56] Guard #317 begins shift
[1518-10-06 00:43] falls asleep
[1518-07-29 00:56] wakes up
[1518-06-18 00:57] falls asleep
[1518-06-20 00:35] wakes up
[1518-04-20 00:03] falls asleep
[1518-07-07 00:48] falls asleep
[1518-06-20 00:58] wakes up
[1518-06-11 00:02] Guard #1597 begins shift
[1518-05-19 00:46] wakes up
[1518-11-06 00:10] falls asleep
[1518-07-30 00:31] wakes up
[1518-05-24 00:51] wakes up
[1518-07-03 00:35] wakes up
[1518-07-14 23:57] Guard #1489 begins shift
[1518-05-30 00:31] wakes up
[1518-10-25 00:20] wakes up
[1518-06-07 00:12] falls asleep
[1518-05-03 00:38] wakes up
[1518-05-17 23:51] Guard #1597 begins shift
[1518-08-17 23:47] Guard #317 begins shift
[1518-09-27 00:53] wakes up
[1518-11-18 00:34] wakes up
[1518-11-16 00:04] Guard #1549 begins shift
[1518-09-14 00:39] wakes up
[1518-11-17 00:02] falls asleep
[1518-06-13 00:42] falls asleep
[1518-08-03 00:02] Guard #181 begins shift
[1518-07-05 23:50] Guard #3121 begins shift
[1518-06-25 00:13] falls asleep
[1518-06-01 00:02] Guard #587 begins shift
[1518-10-21 00:55] falls asleep
[1518-04-06 00:34] falls asleep
[1518-04-25 00:58] wakes up
[1518-11-15 00:02] Guard #3121 begins shift
[1518-11-03 00:49] wakes up
[1518-07-21 00:04] Guard #587 begins shift
[1518-10-14 00:53] wakes up
[1518-03-28 00:36] falls asleep
[1518-09-15 00:03] Guard #1423 begins shift
[1518-03-20 00:43] wakes up
[1518-06-21 00:57] wakes up
[1518-07-19 23:58] Guard #3121 begins shift
[1518-07-14 00:28] falls asleep
[1518-08-15 00:02] Guard #2699 begins shift
[1518-03-24 00:43] falls asleep
[1518-07-16 00:30] falls asleep
[1518-10-18 00:14] falls asleep
[1518-03-25 00:53] falls asleep
[1518-07-03 00:55] wakes up
[1518-06-13 00:00] falls asleep
[1518-06-04 00:25] falls asleep
[1518-03-12 00:00] Guard #2113 begins shift
[1518-03-02 00:04] falls asleep
[1518-04-21 00:01] Guard #1433 begins shift
[1518-10-17 00:47] wakes up
[1518-08-31 00:52] wakes up
[1518-07-11 00:49] falls asleep
[1518-03-23 00:48] wakes up
[1518-05-02 00:29] wakes up
[1518-03-29 23:57] Guard #2699 begins shift
[1518-07-01 00:59] wakes up
[1518-03-22 00:31] falls asleep
[1518-03-26 00:55] wakes up
[1518-04-15 23:58] Guard #1039 begins shift
[1518-10-12 00:59] wakes up
[1518-06-11 00:09] falls asleep
[1518-04-23 00:44] wakes up
[1518-04-17 00:01] Guard #1039 begins shift
[1518-04-24 00:30] falls asleep
[1518-03-27 00:02] Guard #199 begins shift
[1518-08-21 00:22] falls asleep
[1518-07-18 00:57] wakes up
[1518-08-23 00:43] falls asleep
[1518-07-19 00:37] wakes up
[1518-05-22 00:39] falls asleep
[1518-09-25 23:48] Guard #1543 begins shift
[1518-03-27 00:54] wakes up
[1518-09-20 23:57] Guard #1433 begins shift
[1518-10-24 00:59] wakes up
[1518-07-02 00:00] Guard #239 begins shift
[1518-08-17 00:02] Guard #239 begins shift
[1518-09-16 00:24] falls asleep
[1518-10-16 00:14] falls asleep
[1518-10-21 00:48] falls asleep
[1518-05-28 00:09] wakes up
[1518-03-10 00:19] falls asleep
[1518-04-05 00:02] falls asleep
[1518-10-01 00:43] wakes up
[1518-06-28 00:22] wakes up
[1518-04-19 00:20] falls asleep
[1518-08-29 00:56] falls asleep
[1518-08-05 00:32] falls asleep
[1518-07-13 00:55] wakes up
[1518-10-04 23:58] Guard #1423 begins shift
[1518-05-16 23:56] Guard #1549 begins shift
[1518-07-06 00:39] wakes up
[1518-04-17 00:10] falls asleep
[1518-07-06 00:17] falls asleep
[1518-03-08 23:56] Guard #1601 begins shift
[1518-10-10 00:54] wakes up
[1518-09-04 00:59] wakes up
[1518-04-07 00:58] wakes up
[1518-04-02 00:02] Guard #1597 begins shift
[1518-07-03 00:44] falls asleep
[1518-08-02 00:04] Guard #199 begins shift
[1518-10-07 00:22] falls asleep
[1518-08-24 23:58] Guard #1039 begins shift
[1518-08-17 00:34] falls asleep
[1518-03-19 23:59] Guard #181 begins shift
[1518-07-03 23:59] Guard #1597 begins shift
[1518-03-15 00:57] falls asleep
[1518-07-11 00:57] falls asleep
[1518-11-22 00:27] falls asleep
[1518-10-26 00:00] falls asleep
[1518-11-02 00:12] wakes up
[1518-10-27 00:55] wakes up
[1518-09-24 00:20] falls asleep
[1518-06-01 00:15] falls asleep
[1518-07-05 00:23] wakes up
[1518-03-13 00:45] wakes up
[1518-11-23 00:57] wakes up
[1518-05-18 00:01] falls asleep
[1518-06-18 00:59] wakes up
[1518-09-29 00:59] wakes up
[1518-07-27 00:00] Guard #199 begins shift
[1518-05-29 00:00] Guard #1597 begins shift
[1518-04-18 00:59] wakes up
[1518-05-11 00:14] falls asleep
[1518-08-23 00:15] falls asleep
[1518-04-23 00:00] Guard #587 begins shift
[1518-06-07 00:33] wakes up
[1518-05-07 00:59] wakes up
[1518-10-02 00:30] falls asleep
[1518-09-27 00:34] falls asleep
[1518-04-14 00:00] Guard #3121 begins shift
[1518-09-20 00:34] wakes up
[1518-03-28 00:42] wakes up
[1518-11-01 00:40] wakes up
[1518-07-17 00:03] Guard #1543 begins shift
[1518-09-09 00:57] wakes up
[1518-09-23 00:50] wakes up
[1518-05-14 00:27] falls asleep
[1518-04-18 00:00] Guard #2927 begins shift
[1518-03-11 00:17] falls asleep
[1518-08-26 00:07] falls asleep
[1518-04-27 00:54] wakes up
[1518-07-06 00:14] wakes up
[1518-05-24 23:59] Guard #1549 begins shift
[1518-05-12 00:02] Guard #1601 begins shift
[1518-04-06 00:47] wakes up
[1518-11-22 00:06] wakes up
[1518-07-22 00:00] Guard #2699 begins shift
[1518-06-14 00:34] falls asleep
[1518-11-18 23:50] Guard #239 begins shift
[1518-04-04 23:50] Guard #1433 begins shift
[1518-03-08 00:49] falls asleep
|]
