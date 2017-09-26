module Clock (
               Clock
             , Time
             , ClockState (..)
             , Move (..)
             , prettifyClock
             , countdown
             , incrementClock
             , normalizeTime
             , switchMove
             ) where

import Data.List (transpose, replicate, words)
import Data.List.Split (chunksOf)
import NeatNumbers (numbers)
import UI

type Min = String
type Sec = String
type Time = (Min, Sec)

type White = Time
type Black = Time
type Clock = (White, Black)

data ClockState = State { clock :: Clock, move :: Move } deriving (Eq, Show)
data Move = W | B deriving (Eq, Show)

incrementClock :: ClockState -> Int -> ClockState
incrementClock state inc = incremented where
  white = fst $ clock state
  black = snd $ clock state
  incremented = if move state == W
    then State { clock = (normalizeTime $ incTime white inc, normalizeTime black),
                 move = move state }
    else State { clock = (normalizeTime white, normalizeTime $ incTime black inc),
                 move = move state }
         
incTime :: Time -> Int -> Time
incTime time inc = (fst time, show $ read (snd time) + inc)

countdown :: Time -> Time
countdown time = newTime where
  min = read $ fst time
  sec = read $ snd time
  newTime = if sec == 0
    then normalizeTime (show $ min - 1, "59")
    else normalizeTime (show min, show $ sec - 1)

-- bring input to time standarts by correcting it's minutes and seconds
-- (e.g. 00:120 input would be transformed into 02:00)
normalizeTime :: Time -> Time
normalizeTime time = normalized where
  sec = read $ snd time
  min = addZero . show $ read (fst time) + (sec `div` 60)
  normalized = bringToMax (min, addZero . show $ normalizeSec sec)

addZero :: String -> String
addZero str = if length str < 2 then '0' : str else str

bringToMax :: Time -> Time
bringToMax time = if read (fst time) > 99 then ("99", "59") else time

-- bring seconds to time standarts where seconds can't be gt 60.
-- Ignores minutes, we deal with them in normalizeTime func (see above)
-- Serves as helper for normalizeTime func
normalizeSec :: Int -> Int
normalizeSec sec | sec < 60            = sec
                 | (sec `div` 60) == 1 = sec - 60
                 | otherwise           = normalizeSec (sec - 60)
                 
switchMove :: ClockState -> ClockState
switchMove state = if move state == W
  then State { clock = clock state, move = B }
  else State { clock = clock state, move = W }
  
prettifyClock :: Clock -> String
prettifyClock clock = let 
  wMin = map (\num -> numbers !! read num) (chunksOf 1 $ fst $ fst clock)
  wSec = map (\num -> numbers !! read num) (chunksOf 1 $ snd $ fst clock)
  bMin = map (\num -> numbers !! read num) (chunksOf 1 $ fst $ snd clock)
  bSec = map (\num -> numbers !! read num) (chunksOf 1 $ snd $ snd clock)
  in
    concatMap unwords . transpose $
      clockFrame ++ whiteFg ++ wMin ++ colon ++ wSec ++ resetColors
      ++ space ++ redFg ++ bMin ++ colon ++ bSec ++ resetColors ++ columns
