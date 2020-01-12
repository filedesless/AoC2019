module Day04(day04a, day04b) where

import Data.List.Split

import Day04Impl

solve :: (Int -> Bool) -> String -> String
solve p input = let [l, u] = map read (splitOn "-" input) :: [Int] in
  show $ length $ filter (valid p) [l..u]

day04a :: String -> String
day04a = solve (>= 2)

day04b :: String -> String
day04b = solve (== 2)
