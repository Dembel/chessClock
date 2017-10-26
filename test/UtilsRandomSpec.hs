module UtilsRandomSpec (spec) where

import Data.List.Split (splitOn)
import Test.Hspec
import Test.QuickCheck
import Utils

-- ========== parseArgs stuff ========================================
data Args' = Args' { args :: [String] } deriving (Eq, Show)
data WrongArgs' = WrongArgs' { wargs :: [String] } deriving (Eq, Show)
data RandomArgs' = RandomArgs' { rargs :: [String] } deriving (Eq, Show)

instance Arbitrary Args' where
  arbitrary = do
    min <- choose (0, 99) :: Gen Word
    sec <- choose (0, 1000) :: Gen Word
    inc <- choose (0, 1000) :: Gen Word
    return Args' { args = ["-t", show min ++ ":" ++ show sec, "-i", show inc] }

instance Arbitrary WrongArgs' where
  arbitrary = do
    min <- listOf (choose ('a', 'z'))
    sec <- listOf (choose ('a', 'z'))
    inc <- listOf (choose ('a', 'z'))
    return WrongArgs' { wargs = ["-t", min ++ ":" ++ sec, "-i", inc] }

instance Arbitrary RandomArgs' where
  arbitrary = do
    time <- arbitrary :: Gen String
    inc <- arbitrary :: Gen String
    return RandomArgs' { rargs = ["-t", time, "-i", inc] }
    
prop_ParseArgs :: Args' -> Bool
prop_ParseArgs arg = let
  time = splitOn ":" $ args arg !! 1
  inc = read $ args arg !! 3
  in
    parseArgs (args arg) == ((read $ head time, read $ time !! 1), inc)

prop_ParseWrongArgs :: WrongArgs' -> Bool
prop_ParseWrongArgs arg = parseArgs (wargs arg) == ((10, 0), 0)

prop_ParseRandomArgs :: RandomArgs' -> Bool
prop_ParseRandomArgs arg = let
  parsed = parseArgs $ rargs arg
  time = fst parsed
  inc = snd parsed
  in
    ((fst time * 0, snd time * 0), inc * 0) == ((0, 0), 0)
-- ==================================================================

spec :: Spec
spec = do
  describe "random tests" $ do
    it "should pass parseArgs random tests" $ do
      quickCheckWith stdArgs { maxSuccess = 1000 } prop_ParseArgs
      quickCheckWith stdArgs { maxSuccess = 1000 } prop_ParseWrongArgs
      quickCheckWith stdArgs { maxSuccess = 1000 } prop_ParseRandomArgs
