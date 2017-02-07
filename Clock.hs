module Clock (Clock, Time, ClockState(..), Move (..)
             , printClock, countdown, switchMove, incrementClock) where

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

addZero :: String -> String
addZero str = if (length str) < 2 then '0' : str else str

-- toggle curent player to move
switchMove :: ClockState -> ClockState
switchMove state = if move state == W
  then State { clock = clock state, move = B }
  else State { clock = clock state, move = W }
  
-- helper for incrementClock (see below)
incrementTime :: Int -> Time -> Time
incrementTime inc time = incremented where
  min = read $ fst time
  sec = read $ snd time
  incremented = if sec < (60 - inc)
    then (addZero $ show min, addZero . show $ sec + inc)
    else (addZero . show $ min + 1, addZero . show $ (sec + inc) - 60)

-- add increment to the ClockState. Use incrementTime (see above) as helper
incrementClock :: Int -> ClockState -> ClockState
incrementClock inc state = incremented where
  white = fst $ clock state
  black = snd $ clock state
  incremented = if move state == W
    then State { clock = (incrementTime inc white, black), move = W }
    else State { clock = (white, incrementTime inc black), move = B }

-- subtract one second from Time
countdown :: Time -> Time
countdown time = newTime where
  min = read $ fst time :: Int
  sec = read $ snd time :: Int
  newTime = if sec == 0
    then (addZero . show $ min - 1, "59")
    else (addZero $ show min, addZero . show $ sec - 1)

-- take standart Int from Clock and
-- return neat big numbers improving readability
prettifyClock :: Clock -> String
prettifyClock clock = prettifyed where
  colon = [["   ", "   ", "|||", "   ", "   ", "|||", "   ", "   "]]
  space = [x | x <- [chunksOf 1 $ concat $ replicate 8 " "]]
  cols = [x | x <- [chunksOf 1 $ concat $ replicate 8 "\n"]]
  yellowBg = [x | x <- [words $ concat $ replicate 8 "\x1b[37;40m "]]
  blackBg = [x | x <- [words $ concat $ replicate 8 "\x1b[31;40m "]]
  resetColors = [x | x <- [words $ concat $ replicate 8 "\x1b[39;49m "]]
  wMin = map (\num -> numbers !! read num) (chunksOf 1 $ fst $ fst clock)
  wSec = map (\num -> numbers !! read num) (chunksOf 1 $ snd $ fst clock)
  bMin = map (\num -> numbers !! read num) (chunksOf 1 $ fst $ snd clock)
  bSec = map (\num -> numbers !! read num) (chunksOf 1 $ snd $ snd clock)
  transposed = transpose (yellowBg ++ wMin ++ colon ++ wSec ++ resetColors
                          ++ space ++ blackBg ++ bMin ++ colon ++ bSec
                          ++ resetColors ++ cols)
  prettifyed = concatMap unwords transposed

-- print given Clock to stdout
printClock :: Clock -> IO ()
printClock clock = printf "\n%s%s" (prettifyClock clock) backtrack where
  backtrack = (concat $ replicate 9 "\x1b[1A") ++ "\r"
