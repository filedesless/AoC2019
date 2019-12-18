module Day03Spec (spec) where

import Test.Hspec
import Test.QuickCheck

import Cache
import Day03
import Day03Impl


spec :: Spec
spec = do
  let l1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  let l2 = "U62,R66,U55,R34,D71,R55,D58,R83"
  let l3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
  let l4 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

  describe "manDist" $ do
    it "should return the manhattan distance between two points" $ do
      manDist (Pt 0 0 0) (Pt 1 3 0) `shouldBe` 4
      manDist (Pt 1 2 0) (Pt 3 5 0) `shouldBe` 5
      manDist (Pt (-1) 2 0) (Pt 3 5 0) `shouldBe` 7

    it "should work with inversed arguments" $ property $
      \a b c d -> let (p1, p2) = (Pt a b 0, Pt c d 0) in
        manDist p1 p2 == manDist p2 p1

  describe "distFromClosestMan" $
    it "should work on given examples" $ do
      distFromClosestMan (parseLine l1) (parseLine l2) `shouldBe` 159
      distFromClosestMan (parseLine l3) (parseLine l4) `shouldBe` 135

  describe "distFromClosestSignal" $
    it "should work on given examples" $ do
      distFromClosestSignal (parseLine l1) (parseLine l2) `shouldBe` 610
      distFromClosestSignal (parseLine l3) (parseLine l4) `shouldBe` 410

  describe "day03a" $
    it "should have the correct answer" $
      day03a <$> readFile "input/03.txt" >>= verifyAndStore 3 'a' "1017"

  describe "day03b" $
    it "should have the correct answer" $
      day03b <$> readFile "input/03.txt" >>= verifyAndStore 3 'b' "11432"
