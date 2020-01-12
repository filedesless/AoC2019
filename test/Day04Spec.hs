module Day04Spec (spec) where

import Test.Hspec

import Cache
import Day04
import Day04Impl


spec :: Spec
spec = do

  describe "valid" $ do
    it "works with given examples" $ do
      valid (>= 2) 111111 `shouldBe` True
      valid (>= 2) 223450 `shouldBe` False
      valid (>= 2) 123789 `shouldBe` False

    it "works with the new rule" $ do
      valid (== 2) 112233 `shouldBe` True
      valid (== 2) 123444 `shouldBe` False
      valid (== 2) 111122 `shouldBe` True

  describe "day04a" $
    it "should have the correct answer" $
      day04a <$> readFile "input/04.txt" >>= verifyAndStore 4 'a' "2050"

  describe "day04b" $
    it "should have the correct answer" $
      day04b <$> readFile "input/04.txt" >>= verifyAndStore 4 'b' "1390"
