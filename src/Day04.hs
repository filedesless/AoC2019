module Day04(day04a, day04b) where

import Data.List.Split

import Day04Impl

day04a :: String -> String
day04a input = let [l, u] = map read (splitOn "-" input) :: [Int] in
  show $ length $ filter valid [l..u]

day04b :: String -> String
day04b input = undefined
