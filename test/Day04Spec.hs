module Day04Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Cache
import Day04
import Day04Impl


spec :: Spec
spec = do

  describe "valid" $
    it "works with given examples" $ do
      valid 111111 `shouldBe` True
      valid 223450 `shouldBe` False
      valid 123789 `shouldBe` False

  describe "day04a" $
    it "should have the correct answer" $
      day04a <$> readFile "input/04.txt" >>= verifyAndStore 4 'a' "2050"
