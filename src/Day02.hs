module Day02(day02a, day02b) where

import Control.Monad.State
import Data.Foldable
import Data.List.Split
import Data.Sequence

import Day02Impl

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
