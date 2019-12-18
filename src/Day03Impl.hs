module Day03Impl where

import Data.Function
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S

type Direction = (Char, Int)
-- x, y and distance walked
data Point = Pt Int Int Int
  deriving Show
instance Eq Point where
  Pt x1 y1 _ == Pt x2 y2 _ = x1 == x2 && y1 == y2
instance Ord Point where
  Pt x1 y1 _ `compare` Pt x2 y2 _ = (x1, y1) `compare` (x2, y2)

manDist :: Point -> Point -> Int
manDist (Pt x1 y1 _) (Pt x2 y2 _) = ((+) `on` abs) (x2 - x1) (y2 - y1)

parseLine :: String -> [Direction]
parseLine = map (\(h:r) -> (h, read r)). splitOn ","

pointLine :: Point -> Direction -> [Point]
pointLine (Pt x y z) ('R', n) = [ Pt (x + i) y (z + i) | i <- [n,n-1..1] ]
pointLine (Pt x y z) ('L', n) = [ Pt (x - i) y (z + i) | i <- [n,n-1..1] ]
pointLine (Pt x y z) ('U', n) = [ Pt x (y + i) (z + i) | i <- [n,n-1..1] ]
pointLine (Pt x y z) ('D', n) = [ Pt x (y - i) (z + i) | i <- [n,n-1..1] ]

followLines :: Point -> [Point] -> [Direction] -> [Point]
followLines _ _ [] = []
followLines point points (direction:r) =
  let line = pointLine point direction in
    followLines (head line) points r ++ line ++ points

dot :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
dot = (.).(.)

distFromClosestMan :: [Direction] -> [Direction] -> Int
distFromClosestMan = (minimum . S.map (manDist (Pt 0 0 0))) `dot`
  (S.intersection `on` S.fromList . followLines (Pt 0 0 0) [])

distFromClosestSignal :: [Direction] -> [Direction] -> Int
distFromClosestSignal = shortestSignal `on` S.fromList . followLines (Pt 0 0 0) []
  where
    shortestSignal set1 set2 =
      minimum $ catMaybes $ S.toList $ S.map (\pt1@(Pt _ _ z) ->
             if S.member pt1 set2
             then (\(Pt _ _ w) -> z + w) <$> S.lookupGE pt1 set2
             else Nothing) set1
