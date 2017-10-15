module Main where

import Control.Concurrent
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when)
import Control.Exception (catchJust, finally)
import Control.Exception.Base (AsyncException (ThreadKilled))
import Clock
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import System.Console.Terminal.Size (size, Window(..))
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering))
import UI (constructUI)
import Text.Printf (printf)
import Utils (parseArgs)

main = do
  args <- getArgs
  wndSizeMaybe <- size
  let wndSize' = getWndSize wndSizeMaybe
  let params = parseArgs args
  let time = fst params
  let inc = snd params
  printGetReady 10
  finally (catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
                     (mainLoop State { clock = (time, time), move = W,
                                       wndSize = wndSize' } inc)
                     (\e -> return ())) cleanUp

printGetReady :: Int -> IO ()
printGetReady 0 = return ()
printGetReady sec = do
  printf "\x1b[2J\x1b[1;1fGame starts in %d\n" sec
  threadDelay 1000000
  printGetReady (sec - 1)    
    
mainLoop :: ClockState -> Word -> IO ()
mainLoop state inc = do
  mainThreadId <- myThreadId
  stateMVar <- newEmptyMVar
  timerID <- forkFinally (timer state stateMVar)
                         (\e -> when (isLeft e) $ killThread mainThreadId)
  forkIO $ spacebarPressDetector timerID
  lastState <- takeMVar stateMVar
  mainLoop (switchMove $ incrementClock lastState inc) inc

timer :: ClockState -> MVar ClockState -> IO ()
timer State { clock = ((0,0), _)} _ = do
  printf "\x1b[2J\x1b[1;1fBlack won on time\n"
  exitSuccess
timer State { clock = (_, (0, 0))} _ = do
  printf "\x1b[2J\x1b[1;1fWhite won on time\n"
  exitSuccess
timer state mvar = catchJust (\e -> if e == ThreadKilled then Just () else Nothing)
                             runTimer
                             (\_ -> putMVar mvar state) where
  runTimer = do
    wndSizeMaybe <- size
    let wndSize' = getWndSize wndSizeMaybe
    let white = fst $ clock state
    let black = snd $ clock state
    let curMove = move state
    draw state
    threadDelay 1000000
    if curMove == W
      then timer State { clock = (normalizeTime $ countdown white, black)
                       , move = curMove, wndSize = wndSize'} mvar
      else timer State { clock = (white, normalizeTime $ countdown black)
                       , move = curMove, wndSize = wndSize'} mvar

getWndSize :: (Show n, Integral n) => Maybe (Window n) -> (String, String)
getWndSize Nothing = ("101", "25")
getWndSize wnd = (show $ width $ fromJust wnd, show $ height $ fromJust wnd)
                            
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
