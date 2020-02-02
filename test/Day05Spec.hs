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
      \(i, o, v, p, h) -> execState (eval (Str 0)) (h : i, o, fromList [v], p)
                          == (i, o, fromList [h], p + 2)

    it "should work with new Out opcode (given a position)" $ property $
      \(i, o, v, p) -> execState (eval (Out (Pos 0))) (i, o, fromList [v], p)
                       == (i, v : o, fromList [v], p + 2)

    it "should work with new Out opcode (given an immediate)" $ property $
      \(i, o, m, p, v) -> execState (eval (Out (Imm v))) (i, o, m, p)
                          == (i, v : o, m, p + 2)

  describe "run" $
    it "should work with a basic IO program" $ property $
      \(i, o, v) -> execState run (v : i, o, fromList [3,0,4,0,99], 0)
                    == (i, v : o, fromList [v,0,4,0,99], 4)

  -- describe "day05a" $
  --   it "should have the correct answer" $
  --     day05a <$> readFile "input/05.txt" >>= verifyAndStore 4 'a' "2050"

  -- describe "day05b" $
  --   it "should have the correct answer" $
  --     day05b <$> readFile "input/05.txt" >>= verifyAndStore 4 'b' "1390"
