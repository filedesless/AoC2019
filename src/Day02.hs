module Day02(day02a, day02b) where

import Day02Impl
import Data.List.Split
import Control.Monad.State
import Data.Sequence
import Data.Foldable

initial :: String -> Seq Int
initial = fromList . map read . splitOn "," . head . lines

simulate :: Int -> Int -> String -> Int
simulate i j = head . toList . evalState (run 0) . update 1 i . update 2 j . initial

day02a :: String -> String
day02a = show . simulate 12 2

day02b :: String -> String
day02b input = show $ head $ (\(noun, verb, _) -> 100 * noun + verb) <$> valid
  where
    guesses = [ (x, y, simulate x y input) | x <- [0..99], y <- [0..99] ]
    valid = Prelude.filter (\(_, _, r) -> r == 19690720) guesses
