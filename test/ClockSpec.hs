module ClockSpec (spec) where

import Test.Hspec
import Clock

spec :: Spec
spec = do
  describe "normalizeTime" $ do
    it "should normalize the given time to mm:ss standart" $ do
      normalizeTime ("1", "52")  `shouldBe` ("01", "52")
      normalizeTime ("4", "147") `shouldBe` ("06", "27")
      normalizeTime ("4", "1")   `shouldBe` ("04", "01")
      normalizeTime ("02", "59") `shouldBe` ("02", "59")
      normalizeTime ("7", "042") `shouldBe` ("07", "42")
      normalizeTime ("08", "60") `shouldBe` ("09", "00")
      normalizeTime ("67", "44") `shouldBe` ("67", "44")
      normalizeTime ("67", "18000000") `shouldBe` ("99", "59")
      normalizeTime ("99", "60") `shouldBe` ("99", "59")

  describe "countdown" $ do
    it "should subtract one second from time" $ do
      countdown ("1", "25")  `shouldBe` ("01", "24")
      countdown ("0", "54")  `shouldBe` ("00", "53")
      countdown ("10", "74") `shouldBe` ("11", "13")
      countdown ("2", "0")   `shouldBe` ("01", "59")      

  describe "incrementClock" $ do
    it "should increment the clock" $ do
      incrementClock State { clock = (("1", "25"), ("2", "15")), move = W } 2
        `shouldBe` State { clock = (("01", "27"), ("02", "15")), move = W }
      incrementClock State { clock = (("10", "59"), ("2", "54")), move = B } 10
        `shouldBe` State { clock = (("10", "59"), ("03", "04")), move = B }
      incrementClock State { clock = (("4", "59"), ("2", "54")), move = B } 5
        `shouldBe` State { clock = (("04", "59"), ("02", "59")), move = B }

  describe "switchMove" $ do
    it "should correctly switch move when given ClockState" $ do
      switchMove State { clock = (("10", "00"), ("09", "00")), move = W }
        `shouldBe` State { clock = (("10", "00"), ("09", "00")), move = B }
      switchMove State { clock = (("10", "23"), ("09", "45")), move = B }
        `shouldBe` State { clock = (("10", "23"), ("09", "45")), move = W }
