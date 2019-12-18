module Day03Impl where

import Data.Function
import Data.List.Split
import qualified Data.Set as S

type Direction = (Char, Int)
type Point = (Int, Int)

manDist :: Point -> Point -> Int
manDist (x1, y1) (x2, y2) = ((+) `on` abs) (x2 - x1) (y2 - y1)

parseLine :: String -> [Direction]
parseLine = map (\(h:r) -> (h, read r)). splitOn ","

pointLine :: Point -> Direction -> [Point]
pointLine (x, y) ('R', n) = [ (x + i, y) | i <- [n,n-1..1] ]
pointLine (x, y) ('L', n) = [ (x - i, y) | i <- [n,n-1..1] ]
pointLine (x, y) ('U', n) = [ (x, y + i) | i <- [n,n-1..1] ]
pointLine (x, y) ('D', n) = [ (x, y - i) | i <- [n,n-1..1] ]

followLines :: Point -> [Point] -> [Direction] -> [Point]
followLines _ _ [] = []
followLines point points (direction:r) =
  let line = pointLine point direction in
    followLines (head line) points r ++ line ++ points

pointsInCommon :: [Point] -> [Point] -> S.Set Point
pointsInCommon = S.intersection `on` S.fromList

dot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
dot = (.).(.)

distFromClosest :: [Direction] -> [Direction] -> Int
distFromClosest = (minimum . S.map (manDist (0, 0))) `dot`
  (pointsInCommon `on` followLines (0, 0) [])
