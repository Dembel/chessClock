module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Exception (catchJust, finally)
import Control.Exception.Base (AsyncException (ThreadKilled))
import Clock
import Data.Either (isLeft)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, fromMaybe, maybe)
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
  
-- safe constitution for (!!)
elemAt :: Int -> [a] -> Maybe a
elemAt _ []       = Nothing
elemAt 0 (x : _)  = Just x
elemAt i (_ : xs) = elemAt (i - 1) xs

-- check correctness of Time
isTime :: (Maybe Int, Maybe Int) -> Bool
isTime (Nothing, _) = False
isTime (_, Nothing) = False
isTime _            = True

-- convert Time to Int
readTime :: Time -> (Maybe Int, Maybe Int)
readTime time = (min, sec) where
  min = readMaybe (fst time)
  sec = readMaybe (snd time)

-- parse -t argument
getTime :: [String] -> Time
getTime args = time where
  index     = elemIndex "-t" args
  timeMaybe = if isNothing index
              then Just "10:00"
              else elemAt (1 + fromJust index) args
  time      = if isNothing timeMaybe
              then ("10", "00")
              else let rawTime = splitOn ":" $ fromJust timeMaybe in
                if length rawTime < 2 then ("10", "00")
                else (last $ init rawTime, last rawTime)

-- parse -i argument
getIncrement :: [String] -> Maybe Int
getIncrement args = let index = elemIndex "-i" args in
  if isNothing index
  then Just 0
  else let incMaybe = fromMaybe "0" $ elemAt (1 + fromJust index) args in
    readMaybe incMaybe

-- get Time and increment, uses two helper functions above
parseArgs :: [String] -> (Time, Int)
parseArgs args = (time, increment) where
  time = getTime args
  increment = fromMaybe 0 (getIncrement args)

-- get ready message with countdown before starting the clock
printGetReady :: Int -> IO ()
printGetReady 0 = return ()
printGetReady sec = do
  printf "\x1b[2J\x1b[1;1fGame starts in %d\n" sec
  threadDelay 1000000
  printGetReady (sec - 1)

-- check if spacebar button has been pressed. Runs in separate thread
spacebarPressDetector :: ThreadId -> IO ()
spacebarPressDetector timerID = do
  hSetBuffering stdin NoBuffering
  key <- getChar
  if key == ' ' then killThread timerID else spacebarPressDetector timerID

-- clock logic. Runs in separate thread
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

cleanUp :: IO ()
cleanUp = printf "\x1b[?25h"

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
  let time = if isTime $ readTime (fst params)
             then normalizeTime $ fst params
             else ("10", "00")
  printGetReady 15
  finally (catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
                     (mainLoop State { clock = (time, time), move = W } $ snd params)
                     (\e -> return ())) cleanUp
