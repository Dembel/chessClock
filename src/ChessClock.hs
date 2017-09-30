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
import System.Console.Terminal.Size (size, Window(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdin, stdout, BufferMode (NoBuffering))
import UI (constructUI)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (elemAt, isTime)

main = do
  args <- getArgs
  let params = parseArgs args
  let time = if isTime (fst params)
             then normalizeTime (fst params)
             else ("10", "00")
  printGetReady 10
  finally (catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
                     (mainLoop State { clock = (time, time), move = W,
                                       wndSize = ("101", "25")} $ snd params)
                     (\e -> return ())) cleanUp

parseArgs :: [String] -> (Time, Int)
parseArgs args = (time, increment) where
  time = getTime args
  increment = fromMaybe 0 (getIncrement args)

getTime :: [String] -> Time
getTime args = time where
  index = elemIndex "-t" args
  timeMaybe = if isNothing index
              then Just "10:00"
              else args `elemAt` (1 + fromJust index)
  time = if isNothing timeMaybe
         then ("10", "00")
         else let rawTime = splitOn ":" $ fromJust timeMaybe in
           if length rawTime < 2
           then ("10", "00")
           else (last $ init rawTime, last rawTime)

getIncrement :: [String] -> Maybe Int
getIncrement args = let index = elemIndex "-i" args in
  if isNothing index
  then Just 0
  else let incMaybe = fromMaybe "0" $ args `elemAt` (1 + fromJust index) in
    readMaybe incMaybe

printGetReady :: Int -> IO ()
printGetReady 0 = return ()
printGetReady sec = do
  printf "\x1b[2J\x1b[1;1fGame starts in %d\n" sec
  threadDelay 1000000
  printGetReady (sec - 1)    
    
mainLoop :: ClockState -> Int -> IO ()
mainLoop state inc = do
  mainThreadId <- myThreadId
  stateMVar <- newEmptyMVar
  timerID <- forkFinally (timer state stateMVar)
                         (\e -> when (isLeft e) $ killThread mainThreadId)
  forkIO $ spacebarPressDetector timerID
  lastState <- takeMVar stateMVar
  mainLoop (switchMove $ incrementClock lastState inc) inc

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
    wndSizeMaybe <- size
    let wndSize' = ( show $ width $ fromJust wndSizeMaybe
                   , show $ height $ fromJust wndSizeMaybe)
    let white = fst $ clock state
    let black = snd $ clock state
    let curMove = move state
    draw (state)
    threadDelay 1000000
    if curMove == W
      then timer State { clock = (countdown white, black), move = curMove
                       , wndSize = wndSize'} mvar
      else timer State { clock = (white, countdown black), move = curMove
                       , wndSize = wndSize'} mvar
      
spacebarPressDetector :: ThreadId -> IO ()
spacebarPressDetector timerID = do
  hSetBuffering stdin NoBuffering
  key <- getChar
  if key == ' ' then killThread timerID else spacebarPressDetector timerID

draw :: ClockState -> IO ()
draw state = printf "\n%s%s" cls (constructUI state) where
  cls = "\x1b[2J\x1b[?25l"  

cleanUp :: IO ()
cleanUp = printf "\x1b[?25h"
