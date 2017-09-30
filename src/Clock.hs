module Clock (
               Clock
             , Time
             , ClockState (..)
             , Move (..)
             , WindowSize (..)
             , countdown
             , incrementClock
             , normalizeTime
             , switchMove
             ) where

type Min = String
type Sec = String
type Time = (Min, Sec)

type White = Time
type Black = Time
type Clock = (White, Black)

type Width = String
type Height = String
type WindowSize = (Width, Height)

data ClockState = State {
  clock :: Clock,
  move :: Move,
  wndSize :: WindowSize
  } deriving (Eq, Show)

data Move = W | B deriving (Eq, Show)

incrementClock :: ClockState -> Int -> ClockState
incrementClock state inc = incremented where
  white = fst $ clock state
  black = snd $ clock state
  incremented = if move state == W
    then State { clock = (normalizeTime $ incTime white inc, normalizeTime black),
                 move = move state, wndSize = wndSize state }
    else State { clock = (normalizeTime white, normalizeTime $ incTime black inc),
                 move = move state, wndSize = wndSize state }
         
incTime :: Time -> Int -> Time
incTime time inc = (fst time, show $ read (snd time) + inc)

countdown :: Time -> Time
countdown time = newTime where
  min = read $ fst time
  sec = read $ snd time
  newTime = if sec == 0
    then normalizeTime (show $ min - 1, "59")
    else normalizeTime (show min, show $ sec - 1)

normalizeTime :: Time -> Time
normalizeTime time = normalized where
  sec = read $ snd time
  min = addZero . show $ read (fst time) + (sec `div` 60)
  normalized = bringToMax (min, addZero . show $ normalizeSec sec)

addZero :: String -> String
addZero str = if length str < 2 then '0' : str else str

bringToMax :: Time -> Time
bringToMax time = if read (fst time) > 99 then ("99", "59") else time

normalizeSec :: Int -> Int
normalizeSec sec | sec < 60            = sec
                 | (sec `div` 60) == 1 = sec - 60
                 | otherwise           = normalizeSec (sec - 60)
                 
switchMove :: ClockState -> ClockState
switchMove state = if move state == W
  then State { clock = clock state, move = B, wndSize = wndSize state }
  else State { clock = clock state, move = W, wndSize = wndSize state }
