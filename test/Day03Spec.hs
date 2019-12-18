module Day03Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Cache
import Day03
import Day03Impl

spec :: Spec
spec = do
  describe "manDist" $ do
    it "should return the manhattan distance between two points" $ do
      manDist (0, 0) (1, 3) `shouldBe` 4
      manDist (1, 2) (3, 5) `shouldBe` 5
      manDist (-1, 2) (3, 5) `shouldBe` 7

    it "should work with inversed arguments" $ property $
      \p1 p2 -> manDist p1 p2 == manDist p2 p1

  describe "distFromClosest" $
    it "should work on given examples" $ do
      let a = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      let b = "U62,R66,U55,R34,D71,R55,D58,R83"
      let c = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      let d = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      distFromClosest (parseLine a) (parseLine b) `shouldBe` 159
      distFromClosest (parseLine c) (parseLine d) `shouldBe` 135

  describe "day03a" $
    it "should have the correct answer" $
      day03a <$> readFile "input/03.txt" >>= verifyAndStore 3 'a' "1017"
