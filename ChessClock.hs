module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Exception (catchJust)
import Control.Exception.Base (AsyncException (ThreadKilled))
import Clock
import Data.Either (isLeft)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering))
import Text.Printf (printf)
import Text.Read (readMaybe)

-- toggle curent player to move
switchMove :: ClockState -> ClockState
switchMove state = if move state == W
  then State { clock = clock state, move = B }
  else State { clock = clock state, move = W }

-- check if string is number
isNum :: String -> Bool
isNum = all (`elem` "0123456789")

-- check correctness of Time
checkTimeFormat :: Time -> Bool
checkTimeFormat time = isNum (fst time) && isNum (snd time)
  
-- parse -t argument
getTime :: [String] -> [String]
getTime args = let index = elemIndex "-t" args in
  if isNothing index then ["10","00"]
  else splitOn ":" $ (!!) args $ 1 + fromJust index

-- parse -i argument
getIncrement :: [String] -> Maybe Int
getIncrement args = let index = elemIndex "-i" args in
  if isNothing index
  then Just 0
  else readMaybe $ (!!) args $ 1 + fromJust index

-- get Time and increment, uses two helper functions above
parseArgs :: [String] -> (Time, Int)
parseArgs args = ((last $ init time, last time), increment) where
  time = getTime args
  maybeInc = getIncrement args
  increment = fromMaybe 0 maybeInc

-- get ready message with countdown before starting the clock
printGetReady :: Int -> IO ()
printGetReady 0 = return ()
printGetReady sec = do
  printf "\x1b[2J\x1b[1;1fGame starts in %d\n" sec
  threadDelay 1000000
  printGetReady (sec - 1)

-- check if spacebar button has been pressed
spacebarPressDetector :: ThreadId -> IO ()
spacebarPressDetector timerID = do
  hSetBuffering stdin NoBuffering
  key <- getChar
  if key == ' ' then killThread timerID else spacebarPressDetector timerID

timer :: ClockState -> MVar ClockState -> IO ()
timer State { clock = (("00","00"), _)} _ = do
  printf "\x1b[2J\x1b[1;1fBlack won on time\n"
  exitFailure
timer State { clock = (_, ("00","00"))} _ = do
  printf "\x1b[2J\x1b[1;1fWhite won on time\n"
  exitFailure
timer state mvar = catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
  runTimer
  (\_ -> putMVar mvar state) where
    runTimer = do
      let white = fst $ clock state
      let black = snd $ clock state
      let curMove = move state
      printPrettyClock (white, black)
      threadDelay 1000000
      if curMove == W
        then timer State { clock = (countdown white, black), move = curMove } mvar
        else timer State { clock = (white, countdown black), move = curMove } mvar

mainLoop :: ClockState -> Int -> IO ()
mainLoop state inc = do
  mainThreadId <- myThreadId
  stateMVar <- newEmptyMVar
  timerID <- forkFinally (timer state stateMVar)
                         (\e -> when (isLeft e) $ killThread mainThreadId)
  forkIO $ spacebarPressDetector timerID
  lastState <- takeMVar stateMVar
  mainLoop (switchMove $ incrementClock lastState inc) inc

-- initialize
main = do
  args <- getArgs
  let params = parseArgs args
  let time = if checkTimeFormat $ fst params
             then normalizeTime $ fst params
             else ("10", "00")
  printGetReady 15
  catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
            (mainLoop State { clock = (time, time), move = W } $ snd params)
            (\e -> return ())
