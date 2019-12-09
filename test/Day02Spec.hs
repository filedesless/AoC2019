module Day02Spec (spec) where

import Data.Sequence
import Control.Monad.State
import Day02
import Day02Impl
import Test.Hspec

mem1 :: Memory
mem1 = fromList [1,9,10,3,2,3,11,0,99,30,40,50]

mem2 :: Seq Int
mem2 = fromList [1,9,10,70,2,3,11,0,99,30,40,50]

mem3 :: Seq Int
mem3 = fromList [3500,9,10,70,2,3,11,0,99,30,40,50]

spec :: Spec
spec = do
  describe "eval" $
    it "should work with given examples" $ do
      execState (eval (Add (9, 10, 3))) mem1 `shouldBe` mem2
      execState (eval (Mul (3, 11, 0))) mem2 `shouldBe` mem3

  describe "run" $
    it "should work on given examples" $ do
      evalState (run 0) (fromList [1,0,0,0,99]) `shouldBe` fromList [2,0,0,0,99]
      evalState (run 0) (fromList [2,3,0,3,99]) `shouldBe` fromList [2,3,0,6,99]
      evalState (run 0) (fromList [2,4,4,5,99,0]) `shouldBe` fromList [2,4,4,5,99,9801]
      let (bef, aft) = (fromList [1,1,1,4,99,5,6,0,99], fromList [30,1,1,4,2,5,6,0,99])
      evalState (run 0) bef `shouldBe` aft

  describe "day02a" $
    it "should have the correct answer" $
      day02a <$> readFile "input/02.txt" `shouldReturn` "5110675"
