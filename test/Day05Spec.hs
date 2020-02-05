module Day05Spec (spec) where

import Control.Monad.State
import Data.Sequence
import Test.Hspec
import Test.QuickCheck

import Cache
import Day05
import Day05Impl
import Day02Impl


spec :: Spec
spec = do

  describe "eval" $ do
    it "should work with new Str opcode" $ property $
      \(i, o, v, p, h) -> let Just s = eval (Str 0) in
        execState s (h : i, o, fromList [v], p) == (i, o, fromList [h], p + 2)

    it "should work with new Out opcode (given a position)" $ property $
      \(i, o, v, p) -> let Just s = eval (Out (Pos 0)) in
        execState s (i, o, fromList [v], p) == (i, v : o, fromList [v], p + 2)

    it "should work with new Out opcode (given an immediate)" $ property $
      \(i, o, m, p, v) -> let Just s = eval (Out (Imm v)) in
        execState s (i, o, m, p) == (i, v : o, m, p + 2)

  describe "run" $ do
    it "should work with a basic IO program" $ property $
      \(i, o, v) -> execState run (v : i, o, fromList [3,0,4,0,99], 0)
                    == (i, v : o, fromList [v,0,4,0,99], 4)

    it "should work on given example" $
      execState run ([], [], fromList [1002,4,3,4,33], 0)
      `shouldBe` ([], [], fromList [1002, 4, 3, 4, 99], 4)

  describe "parseOp" $ do
    it "should parse old opcodes correctly" $ do
      parseOp (fromList [1,9,10,3]) 0 `shouldBe` Just (Add (Pos 9, Pos 10, 3))
      parseOp (fromList [2,3,11,0]) 0 `shouldBe` Just (Mul (Pos 3, Pos 11, 0))
      parseOp (fromList [99]) 0 `shouldBe` Just Halt

    it "should parse new opcodes correctly" $ do
      parseOp (fromList [3, 0]) 0 `shouldBe` Just (Str 0)
      parseOp (fromList [4, 0]) 0 `shouldBe` Just (Out (Pos 0))

    it "should parse operand modes correctly" $
      parseOp (fromList [1002,4,3,4]) 0 `shouldBe` Just (Mul (Pos 4, Imm 3, 4))

  -- describe "day05a" $
  --   it "should have the correct answer" $
  --     day05a <$> readFile "input/05.txt" >>= verifyAndStore 4 'a' "2050"

  -- describe "day05b" $
  --   it "should have the correct answer" $
  --     day05b <$> readFile "input/05.txt" >>= verifyAndStore 4 'b' "1390"
