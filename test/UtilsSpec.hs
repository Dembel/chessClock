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

  describe "parseArgs" $ do
    it "should parse Time and increment from stdin" $ do
      parseArgs ["-t", "10:47", "-i", "86"] `shouldBe` ((10, 47), 86)
      parseArgs ["-e", "10:47", "-i", "ased"] `shouldBe` ((10, 0), 0)
      parseArgs ["-i", "1", "-t", "12"] `shouldBe` ((10, 0), 1)
      parseArgs ["-i", "-t", "1:er"] `shouldBe` ((1, 0), 0)
      parseArgs ["-i", "-t", "-i", "4", "-t", "1:14"] `shouldBe` ((10, 0), 0)
      parseArgs ["-i", "4e", "-t", "q1:14"] `shouldBe` ((10, 14), 0)
      parseArgs ["-t"] `shouldBe` ((10, 00), 0)
      parseArgs [] `shouldBe` ((10, 00), 0)
      parseArgs [""] `shouldBe` ((10, 00), 0)
