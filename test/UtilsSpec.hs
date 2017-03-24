module UtilsSpec (spec) where

import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "elemAt" $ do
    it "should safely get an element of the list at given index" $ do
      [1, 2, 3, 4, 5] `elemAt` 2 `shouldBe` Just 3
      [1, 2, 3, 4, 5] `elemAt` 10 `shouldBe` Nothing
      ["foo", "bar", "foobar"] `elemAt` 2 `shouldBe` Just "foobar"
      [True, False] `elemAt` 0 `shouldBe` Just True

  describe "isTime" $ do
    it "should correctly check if given tuple is Time or not" $ do
      isTime ("a", "25") `shouldBe` False
      isTime ("10", "27") `shouldBe` True
      isTime ("1", "2a") `shouldBe` False
      isTime ("1", "2") `shouldBe` True
      isTime ("01", "60") `shouldBe` True
      isTime ("bd", "ssdd") `shouldBe` False
