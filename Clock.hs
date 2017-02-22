module Clock (Clock, Time, ClockState (..), Move (..)
             , printPrettyClock, countdown, incrementClock, normalizeTime) where

import Data.List (transpose, replicate, words)
import Data.List.Split (chunksOf)
import NeatNumbers (numbers)
import Text.Printf (printf)

type Min = String
type Sec = String
type Time = (Min, Sec)

type White = Time
type Black = Time
type Clock = (White, Black)

data ClockState = State { clock :: Clock, move :: Move }
data Move = W | B deriving Eq

{- ******************** clock-normalizing stuff ******************** -}

-- add zero to minutes or seconds if there's only one digit to conform to
-- digital clock standarts (e.g. 2-digit numbers)
addZero :: String -> String
addZero str = if length str < 2 then '0' : str else str

-- bring seconds to time standarts where seconds can't be gt 60.
-- Ignores minutes, we deal with them in normalizeTime func (see below)
-- Serves as helper for normalizeTime func
normalizeSec :: Int -> Int
normalizeSec sec | sec < 60            = sec
                 | (sec `div` 60) == 1 = sec - 60
                 | otherwise           = normalizeSec (sec - 60)

-- bring input to time standarts by correcting it's minutes and seconds
-- (e.g. 00:120 input would be transformed into 02:00 etc.)
normalizeTime :: Time -> Time
normalizeTime time = normalized where
  sec = read $ snd time
  min = addZero . show $ read (fst time) + (sec `div` 60)
  normalized = (min, addZero . show $ normalizeSec sec)
               
{- ********************************************************************** -}
{- ******************** operations on clock ***************************** -}

-- helper for incrementClock (see below)
incrementTime :: Time -> Int -> Time
incrementTime time inc = normalizeTime (min, sec) where
  min = fst time
  sec = show $ read (snd time) + inc

-- add increment to the ClockState. Uses incrementTime (see above) as helper
incrementClock :: ClockState -> Int -> ClockState
incrementClock state inc = incremented where
  white = fst $ clock state
  black = snd $ clock state
  incremented = if move state == W
    then State { clock = (incrementTime white inc, black), move = move state }
    else State { clock = (white, incrementTime black inc), move = move state }

-- subtract one second from Time
countdown :: Time -> Time
countdown time = newTime where
  min = read $ fst time :: Int
  sec = read $ snd time :: Int
  newTime = if sec == 0
    then (addZero . show $ min - 1, "59")
    else (addZero $ show min, addZero . show $ sec - 1)

-- take Time as Int from Clock and
-- replace digits with neat big numbers improving readability
prettifyClock :: Clock -> String
prettifyClock clock = prettifyed where
  colon = [["   ", "   ", "|||", "   ", "   ", "|||", "   ", "   "]]
  space = [chunksOf 1 $ concat $ replicate 8 " "]
  columns = [chunksOf 1 $ concat $ replicate 8 "\n"]
  whiteFg = [words $ concat $ replicate 8 "\x1b[37;40m "]
  redFg = [words $ concat $ replicate 8 "\x1b[31;40m "]
  resetColors = [words $ concat $ replicate 8 "\x1b[39;49m "]
  wMin = map (\num -> numbers !! read num) (chunksOf 1 $ fst $ fst clock)
  wSec = map (\num -> numbers !! read num) (chunksOf 1 $ snd $ fst clock)
  bMin = map (\num -> numbers !! read num) (chunksOf 1 $ fst $ snd clock)
  bSec = map (\num -> numbers !! read num) (chunksOf 1 $ snd $ snd clock)
  prettifyed = concatMap unwords $ transpose $
               whiteFg ++ wMin ++ colon ++ wSec ++ resetColors ++ space ++ redFg
               ++ bMin ++ colon ++ bSec ++ resetColors ++ columns

{- ********************************************************************** -}

-- print given Clock to stdout
printPrettyClock :: Clock -> IO ()
printPrettyClock clock = printf "\n%s%s" backtrack (prettifyClock clock) where
  backtrack = "\x1b[2J\x1b[12;1f"
