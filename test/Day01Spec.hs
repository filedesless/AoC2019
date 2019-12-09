module Day01Spec (spec) where

import Day01
import Day01Impl
import Test.Hspec

spec :: Spec
spec = do
  describe "fuel" $
    it "should work with given examples" $ do
      fuel 12 `shouldBe` 2
      fuel 14 `shouldBe` 2
      fuel 1969 `shouldBe` 654
      fuel 100756 `shouldBe` 33583

  describe "day01a" $
    it "should have the correct answer" $
      day01a <$> readFile "input/01.txt" `shouldReturn` "3331849"

  describe "fuel'" $
    it "should work with given examples" $ do
      fuel' 14 `shouldBe` 2
      fuel' 1969 `shouldBe` 966
      fuel' 100756 `shouldBe` 50346

  describe "day01b" $
    it "should have the correct answer" $ do
      day01b <$> (readFile "input/01.txt") `shouldReturn` "4994898"
