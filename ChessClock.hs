module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (catchJust)
import Control.Exception.Base (AsyncException (ThreadKilled))
import Data.Either (isLeft)
import Data.List (findIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering))
import Text.Printf (printf)

type Min = String
type Sec = String
type Time = (Min, Sec)

type White = Time
type Black = Time
type Clock = (White, Black)

data Move = W | B deriving Eq
data ClockState = State { clock :: Clock, move :: Move }

-- syntactic sugar for threadDelay function, use seconds instead of microseconds
threadDelaySec :: Int -> IO ()
threadDelaySec sec = threadDelay (10 ^ 6 * sec)

-- toggle curent player to move
switchMove :: ClockState -> ClockState
switchMove state = if move state == W
  then State { clock = clock state, move = B }
  else State { clock = clock state, move = W }

addZero :: String -> String
addZero str = if (length str) < 2 then '0' : str else str

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

getTime :: [String] -> [String]
getTime args = let index = findIndex (== "-t") args in
  if isNothing index then ["10","00"]
  else splitOn ":" $ (!!) args $ 1 + (fromJust index)

getIncrement :: [String] -> Int
getIncrement args = let index = findIndex (== "-i") args in
  if isNothing index then 0 else read $ (!!) args $ 1 + (fromJust index) :: Int

parseArgs :: [String] -> (Time, Int)
parseArgs args = ((head time, last time), increment) where
  time = getTime args
  increment = getIncrement args  

-- print given Clock to stdout
printClock :: Clock -> IO ()
printClock clock = printf message white black where
  message = "\x1b[30;47m %s \x1b[39;49m  \x1b[37;40m %s \x1b[39;49m\n\x1b[1A\r"
  white = (fst $ fst clock) ++ ":" ++ (snd $ fst clock)
  black = (fst $ snd clock) ++ ":" ++ (snd $ snd clock)

-- check if spacebar button has been pressed
spacebarPressDetector :: ThreadId -> IO ()
spacebarPressDetector timerID = do
  hSetBuffering stdin NoBuffering
  key <- getChar
  if key == ' ' then killThread timerID else spacebarPressDetector timerID

timer :: ClockState -> MVar ClockState -> IO ()
timer State { clock = (("00","00"), _)} _ = do
  putStrLn "Black won on time"
  exitFailure
timer State { clock = (_, ("00","00"))} _ = do
  putStrLn "White won on time"
  exitFailure
timer state mvar = do
  catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
            (runTimer)
            (\e -> putMVar mvar state) where
  runTimer = do
    let white = fst $ clock state
    let black = snd $ clock state
    let curMove = move state
    printClock (white , black)
    threadDelaySec 1
    if curMove == W
      then timer State { clock = (countdown white, black), move = curMove } mvar
      else timer State { clock = (white, countdown black), move = curMove } mvar

mainLoop :: ClockState -> Int -> IO ()
mainLoop state inc = do
  mainThreadId <- myThreadId
  stateMVar <- newEmptyMVar
  timerID <- forkFinally (timer state stateMVar)
                         (\e -> if isLeft e
                                then killThread mainThreadId
                                else return ())
  forkIO $ spacebarPressDetector timerID
  lastState <- takeMVar stateMVar
  mainLoop (switchMove $ incrementClock inc lastState) inc

-- initialize
main = do
  args <- getArgs
  let params = parseArgs args
  let newClock = (fst params, fst params)
  catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
            (mainLoop State { clock = newClock, move = W } $ snd params)
            (\e -> return ())
