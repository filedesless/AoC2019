module Day02Spec (spec) where

import Control.Monad.State
import Data.Sequence
import Test.Hspec

import Cache
import Day02
import Day02Impl

mem1 :: Memory
mem1 = fromList [1,9,10,3,2,3,11,0,99,30,40,50]

mem2 :: Memory
mem2 = fromList [1,9,10,70,2,3,11,0,99,30,40,50]

mem3 :: Memory
mem3 = fromList [3500,9,10,70,2,3,11,0,99,30,40,50]

s :: Memory -> Computer
s m = ([], [], m, 0)

spec :: Spec
spec = do
  describe "eval" $
    it "should work with given examples" $ do
      execState (eval (Add (Pos 9, Pos 10, 3))) (s mem1)
        `shouldBe` ([], [], mem2, 4)
      execState (eval (Mul (Pos 3, Pos 11, 0))) (s mem2)
        `shouldBe` ([], [], mem3, 4)

  describe "run" $
    it "should work on given examples" $ do
      evalState run (s (fromList [1,0,0,0,99]))
        `shouldBe` fromList [2,0,0,0,99]
      evalState run (s (fromList [2,3,0,3,99]))
        `shouldBe` fromList [2,3,0,6,99]
      evalState run (s (fromList [2,4,4,5,99,0]))
        `shouldBe` fromList [2,4,4,5,99,9801]
      let (bef, aft) =
            (fromList [1,1,1,4,99,5,6,0,99], fromList [30,1,1,4,2,5,6,0,99])
      evalState run (s bef) `shouldBe` aft

  describe "day02a" $
    it "should have the correct answer" $
      day02a <$> readFile "input/02.txt" >>= verifyAndStore 2 'a' "5110675"

  describe "day02b" $
    it "should have the correct answer" $
      day02b <$> readFile "input/02.txt" >>= verifyAndStore 2 'b' "4847"
