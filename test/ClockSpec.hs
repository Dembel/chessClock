module ClockSpec (spec) where

import Test.Hspec
import Clock

spec :: Spec
spec = do
  describe "normalizeTime" $ do

    it "should normalize the given time to mm:ss standart" $ do
      normalizeTime (1, 52)  `shouldBe` (1, 52)
      normalizeTime (4, 147) `shouldBe` (6, 27)
      normalizeTime (4, 1)   `shouldBe` (4, 1)
      normalizeTime (2, 59) `shouldBe` (2, 59)
      normalizeTime (7, 42) `shouldBe` (7, 42)
      normalizeTime (8, 60) `shouldBe` (9, 0)
      normalizeTime (67, 44) `shouldBe` (67, 44)
      normalizeTime (67, 18000000) `shouldBe` (99, 59)
      normalizeTime (99, 60) `shouldBe` (99, 59)

  describe "countdown" $ do
    it "should subtract one second from time" $ do
      countdown (1, 25)  `shouldBe` (1, 24)
      countdown (0, 54)  `shouldBe` (0, 53)
      countdown (10, 74) `shouldBe` (10, 73)
      countdown (2, 0)   `shouldBe` (1, 59)

  describe "incrementClock" $ do
    it "should increment the clock" $ do
      incrementClock State { clock = ((1, 25), (2, 15))
                           , move = W, wndSize = ("1", "1") } 2
        `shouldBe` State { clock = ((1, 27), (2, 15))
                         , move = W, wndSize = ("1", "1") }
      incrementClock State { clock = ((10, 59), (2, 54))
                           , move = B, wndSize = ("1", "1") } 10
        `shouldBe` State { clock = ((10, 59), (3, 4))
                         , move = B, wndSize = ("1", "1") }
      incrementClock State { clock = ((4, 59), (2, 54))
                           , move = B, wndSize = ("1", "1") } 5
        `shouldBe` State { clock = ((4, 59), (2, 59))
                         , move = B, wndSize = ("1", "1") }

  describe "switchMove" $ do
    it "should correctly switch move when given ClockState" $ do
      switchMove State { clock = ((10, 0), (9, 0)), move = W
                       , wndSize = ("1", "1") }
        `shouldBe` State { clock = ((10, 0), (9, 0)), move = B
                         , wndSize = ("1", "1") }
      switchMove State { clock = ((10, 23), (9, 45)), move = B
                       , wndSize = ("1", "1") }
        `shouldBe` State { clock = ((10, 23), (9, 45)), move = W
                         , wndSize = ("1", "1") }
