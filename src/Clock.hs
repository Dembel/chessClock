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
incrementClock state@State { move = W } inc =
  state { clock = (normalizeTime $ incTime (fst $ clock state) inc
                  , normalizeTime . snd $ clock state) }
incrementClock state@_ inc =  
  state { clock = (normalizeTime . fst $ clock state
                  , normalizeTime $ incTime (snd $ clock state) inc) }
  
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
switchMove state@State { move = W } = state { move = B }
switchMove state@State { move = B } = state { move = W }
