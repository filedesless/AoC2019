module Day04Impl where

import Data.List

digits :: Int -> [Int]
digits = map (read . return) . show

sameAdjacents :: (Int -> Bool) -> [Int] -> Bool
sameAdjacents p = any (p . length) . group

neverDecrease :: [Int] -> Bool
neverDecrease [ ] = True
neverDecrease [_] = True
neverDecrease (a:b:r) = a <= b && neverDecrease (b : r)

valid :: (Int -> Bool) -> Int -> Bool
valid p n = let d = digits n in
  sameAdjacents p d && neverDecrease d
