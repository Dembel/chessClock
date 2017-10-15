module UI (constructUI) where

import Clock (Clock, ClockState(..), Move(..), WindowSize(..))
import Data.List (transpose, replicate)
import Digits (drawNum)

type UIElement = [[String]]
type X = Word
type Y = Word

constructUI :: ClockState -> String
constructUI state = concatMap unwords (transpose ui) ++ "\n" where
  centerX = read (fst $ wndSize state) `div` 2
  centerY = read (snd $ wndSize state) `div` 2
  moveFlag' = moveFlag (centerX - 47 + 22, centerY - 5) $ move state
  clock' = constructClock (centerX - 45) (centerY - 3) $ clock state  
  ui = moveFlag' ++ clockFrame (centerX - 47) (centerY - 4) ++ clock'

constructClock :: X -> Y -> Clock -> UIElement
constructClock x y clock = let 
  wMin = drawNum (fst $ fst clock)
  wSec = drawNum (snd $ fst clock)
  bMin = drawNum (fst $ snd clock)
  bSec = drawNum (snd $ snd clock)
  columns = [replicate 7 $ "\x1b[1E\x1b[" ++ show x ++ "C"]
  in
    setCursor (x + 2) y ++ whiteFg ++ wMin ++ colon ++ wSec ++ resetColors
    ++ space ++ redFg ++ bMin ++ colon ++ bSec ++ resetColors ++ columns

setCursor :: X -> Y -> UIElement
setCursor x y = [["\x1b[" ++ show y ++ ";" ++ show x ++ "f"]]

space :: UIElement
space = [words $ concat $ replicate 8 "\x1b[1C "]

whiteFg :: UIElement
whiteFg = [words $ concat $ replicate 8 "\x1b[37;40m "]

whiteFgNoBg :: UIElement
whiteFgNoBg = [words $ concat $ replicate 8 "\x1b[37m "]

redFg :: UIElement
redFg = [words $ concat $ replicate 8 "\x1b[31;40m "]

redFgNoBg :: UIElement
redFgNoBg = [words $ concat $ replicate 8 "\x1b[31m "]

resetColors :: UIElement
resetColors = [words $ concat $ replicate 8 "\x1b[39;49m "]

clockFrame :: X -> Y -> UIElement
clockFrame x y =
  setCursor x y ++ [[concat $ replicate 93 "="]]
  ++ setCursor x (y + 1) ++ [["=="]] ++ setCursor (x + 91) (y + 1) ++ [["=="]]
  ++ setCursor (x + 46) (y + 1) ++ [["="]]
  ++ setCursor x (y + 2) ++ [["=="]] ++ setCursor (x + 91) (y + 2) ++ [["=="]]
  ++ setCursor (x + 46) (y + 2) ++ [["="]]  
  ++ setCursor x (y + 3) ++ [["=="]] ++ setCursor (x + 91) (y + 3) ++ [["=="]]
  ++ setCursor (x + 46) (y + 3) ++ [["="]]
  ++ setCursor x (y + 4) ++ [["=="]] ++ setCursor (x + 91) (y + 4) ++ [["=="]]
  ++ setCursor (x + 46) (y + 4) ++ [["="]]
  ++ setCursor x (y + 5) ++ [["=="]] ++ setCursor (x + 91) (y + 5) ++ [["=="]]
  ++ setCursor (x + 46) (y + 5) ++ [["="]]  
  ++ setCursor x (y + 6) ++ [["=="]] ++ setCursor (x + 91) (y + 6) ++ [["=="]]
  ++ setCursor (x + 46) (y + 6) ++ [["="]]  
  ++ setCursor x (y + 7) ++ [["=="]] ++ setCursor (x + 91) (y + 7) ++ [["=="]]
  ++ setCursor (x + 46) (y + 7) ++ [["="]]  
  ++ setCursor x (y + 8) ++ [["=="]] ++ setCursor (x + 91) (y + 8) ++ [["=="]]
  ++ setCursor (x + 46) (y + 8) ++ [["="]]  
  ++ setCursor x (y + 9) ++ [[concat $ replicate 93 "="]] 

colon :: UIElement
colon = [[ "   "
         , "   "
         , "|||"
         , "   "
         , "   "
         , "|||"
         , "   "
         , "   "]]

moveFlag :: (X, Y) -> Move -> UIElement
moveFlag (x,y) W = setCursor x y ++ whiteFgNoBg ++ [["####"]] ++ resetColors
moveFlag (x,y) B = setCursor (x + 44) y ++ redFgNoBg ++ [["####"]] ++ resetColors
