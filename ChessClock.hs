module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (catchJust)
import Control.Exception.Base (AsyncException (ThreadKilled))
import Clock
import Data.Either (isLeft)
import Data.List (findIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering))
import Text.Printf (printf)
import Text.Read (readMaybe)

-- check if string consists of numbers only
isNum :: String -> Bool
isNum = all (\t -> t `elem` "0123456789")

-- check correctness of Time
checkTimeFormat :: Time -> Bool
checkTimeFormat time = if (isNum $ fst time) && (isNum $ snd time)
  then True
  else False
  
getTime :: [String] -> [String]
getTime args = let index = findIndex (== "-t") args in
  if isNothing index then ["10","00"]
  else splitOn ":" $ (!!) args $ 1 + (fromJust index)

getIncrement :: [String] -> Maybe Int
getIncrement args = let index = findIndex (== "-i") args in
  if isNothing index
  then Just 0
  else readMaybe $ (!!) args $ 1 + (fromJust index)

-- get Time and increment, uses two helper functions above
parseArgs :: [String] -> (Time, Int)
parseArgs args = ((head time, last time), increment) where
  time = getTime args
  maybeInc = getIncrement args
  increment = if isNothing maybeInc then 0 else fromJust maybeInc

-- check if spacebar button has been pressed
spacebarPressDetector :: ThreadId -> IO ()
spacebarPressDetector timerID = do
  hSetBuffering stdin NoBuffering
  key <- getChar
  if key == ' ' then killThread timerID else spacebarPressDetector timerID

-- syntactic sugar for threadDelay function, use seconds instead of microseconds
threadDelaySec :: Int -> IO ()
threadDelaySec sec = threadDelay (10 ^ 6 * sec)

timer :: ClockState -> MVar ClockState -> IO ()
timer State { clock = (("00","00"), _)} _ = do
  printf "\x1b[J%s\n" "Black won on time"
  exitFailure
timer State { clock = (_, ("00","00"))} _ = do
  printf "\x1b[J%s\n" "White won on time"
  exitFailure
timer state mvar = do
  catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
            (runTimer)
            (\_ -> putMVar mvar state) where
  runTimer = do
    let white = fst $ clock state
    let black = snd $ clock state
    let curMove = move state
    printClock (white, black)
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
  let time = if checkTimeFormat $ fst params
             then normalizeTime $ fst params
             else ("10", "00")
  catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
            (mainLoop State { clock = (time, time), move = W } $ snd params)
            (\e -> return ())
