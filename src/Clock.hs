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

type Min = Word
type Sec = Word
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

incrementClock :: ClockState -> Word -> ClockState
incrementClock state@State { clock = _, move = W, wndSize = _ } inc =
  State { clock = (normalizeTime $ incTime (fst $ clock state) inc
                  , normalizeTime . snd $ clock state)
        , move = move state, wndSize = wndSize state }
incrementClock state@State { clock = _, move = B, wndSize = _ } inc =  
  State { clock = (normalizeTime . fst $ clock state
                  , normalizeTime $ incTime (snd $ clock state) inc)
        , move = move state, wndSize = wndSize state }
  
incTime :: Time -> Word -> Time
incTime time inc = (fst time, snd time + inc)

countdown :: Time -> Time
countdown time@(_, 0) = (fst time - 1, 59)
countdown time = (fst time, snd time - 1)
  
normalizeTime :: Time -> Time
normalizeTime time = normalized where
  sec = snd time
  min = fst time + (sec `div` 60)
  normalized = bringToMax (min, normalizeSec sec)

bringToMax :: Time -> Time
bringToMax time = if fst time > 99 then (99, 59) else time

normalizeSec :: Word -> Word
normalizeSec sec | sec < 60            = sec
                 | (sec `div` 60) == 1 = sec - 60
                 | otherwise           = normalizeSec (sec - 60)
                 
switchMove :: ClockState -> ClockState
switchMove state = if move state == W
  then State { clock = clock state, move = B, wndSize = wndSize state }
  else State { clock = clock state, move = W, wndSize = wndSize state }
