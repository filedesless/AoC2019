module Day01Impl where

fuel :: Int -> Int
fuel mass = div mass 3 - 2

fuel' :: Int -> Int
fuel' mass
  | more <= 0 = 0
  | otherwise = more + fuel' more
  where more = fuel mass
