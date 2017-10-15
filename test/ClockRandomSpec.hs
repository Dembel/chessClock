module ClockRandomSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Clock

data Timer = Timer { m :: Word, s :: Word } deriving (Eq, Show)

instance Arbitrary Timer where
  arbitrary = do
    min <- choose (1, 1000) :: Gen Word
    sec <- choose (0, 1000) :: Gen Word   
    return Timer { m = min, s = sec }

prop_Countdown :: Timer -> Bool
prop_Countdown tm@Timer { m = _, s = 0 } = countdown (m tm, s tm) == (m tm - 1, 59)
prop_Countdown tm = countdown (m tm, s tm) == (m tm, s tm - 1)

spec :: Spec
spec = do
  describe "random tests" $ do
    it "should pass countdown random tests" $ do
      quickCheckWith stdArgs { maxSuccess = 10000 } prop_Countdown
