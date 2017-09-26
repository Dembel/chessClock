module UI (
            clockFrame
          , colon
          , space
          , columns
          , whiteFg
          , redFg
          , resetColors
          ) where

import Data.List.Split (chunksOf)

sideTile :: String
sideTile = "**  "

clockFrame :: [[String]]
clockFrame = [chunksOf (length sideTile) $ concat $ replicate 8 sideTile]

wrapIn :: [[String]] -> [[String]] -> [[String]]
wrapIn val wraper = undefined

colon :: [[String]]
colon = [["   ", "   ", "|||", "   ", "   ", "|||", "   ", "   "]]

space :: [[String]]
space = [chunksOf 1 $ concat $ replicate 10 " "]

columns :: [[String]]
columns = [chunksOf 1 $ concat $ replicate 10 "\n"]

whiteFg :: [[String]]
whiteFg = [words $ concat $ replicate 8 "\x1b[37;40m "]

redFg :: [[String]]
redFg = [words $ concat $ replicate 8 "\x1b[31;40m "]

resetColors :: [[String]]
resetColors = [words $ concat $ replicate 8 "\x1b[39;49m "]
