module Utils (elemAt, parseArgs, getWndSize) where

import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import System.Console.Terminal.Size (size, Window(..))
import Text.Read (readMaybe)

parseArgs :: [String] -> ((Word, Word), Word)
parseArgs args = (getTime args, getIncrement args)

getTime :: [String] -> (Word, Word)
getTime args = time where
  index = fromMaybe 999 $ elemIndex "-t" args
  timeMaybe = fromMaybe "10:00" $ args `elemAt` (1 + index)
  time = let rawTime = splitOn ":" timeMaybe in
    if length rawTime < 2
    then (10, 0)
    else (fromMaybe 10 $ readMaybe $ last $ init rawTime
         , fromMaybe 0 $ readMaybe $ last rawTime)

getIncrement :: [String] -> Word
getIncrement args = let index = fromMaybe 999 $ elemIndex "-i" args in
  fromMaybe 0 $ readMaybe $ fromMaybe "0" $ args `elemAt` (1 + index)

getWndSize :: (Show n, Integral n) => Maybe (Window n) -> (String, String)
getWndSize Nothing = ("101", "25")
getWndSize wnd = (show $ width $ fromJust wnd, show $ height $ fromJust wnd)  

elemAt :: [a] -> Int -> Maybe a
elemAt [] _ = Nothing
elemAt (x : _) 0 = Just x
elemAt (_ : xs) i = elemAt xs (i - 1)
