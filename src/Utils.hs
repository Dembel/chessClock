module Utils (
    elemAt
  , isTime
  ) where

import Data.Maybe (isNothing)
import Text.Read (readMaybe)

-- safe constitution for (!!)
elemAt :: [a] -> Int -> Maybe a
elemAt [] _ = Nothing
elemAt (x : _) 0 = Just x
elemAt (_ : xs) i = elemAt xs (i - 1)

-- check correctness of Time
isTime :: (String, String) -> Bool
isTime time = not (isNothing min || isNothing sec) where
  min = readMaybe (fst time) :: Maybe Int
  sec = readMaybe (snd time) :: Maybe Int
