module Day02(day02a, day02b) where

import Control.Monad.State
import Data.Foldable
import Data.List.Split
import Data.Sequence

import Day02Impl

initial :: Int -> Int -> String -> Computer
initial i j s = ([], [], update 1 i $ update 2 j mem, 0)
  where mem = fromList . map read . splitOn "," . head $ lines s

simulate :: Int -> Int -> String -> Int
simulate i j = head . toList . evalState run . initial i j

day02a :: String -> String
day02a = show . simulate 12 2

day02b :: String -> String
day02b input = show $ head $ (\(noun, verb, _) -> 100 * noun + verb) <$> valid
  where
    guesses = [ (x, y, simulate x y input) | x <- [0..99], y <- [0..99] ]
    valid = Prelude.filter (\(_, _, r) -> r == 19690720) guesses
