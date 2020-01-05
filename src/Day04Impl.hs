module Day04Impl where

digits :: Int -> [Int]
digits = map (read . return) . show

sameAdjacents :: [Int] -> Bool
sameAdjacents [ ] = False
sameAdjacents [_] = False
sameAdjacents (a:b:r) = a == b || sameAdjacents (b : r)

neverDecrease :: [Int] -> Bool
neverDecrease [ ] = True
neverDecrease [_] = True
neverDecrease (a:b:r) = a <= b && neverDecrease (b : r)

valid :: Int -> Bool
valid n = let d = digits n in
  sameAdjacents d && neverDecrease d
