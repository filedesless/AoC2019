module Day05(day05a, day05b) where

import Control.Monad.State
import Data.List.Split
import Data.Sequence

import Day05Impl
import Day02Impl

initial :: String -> Computer
initial s = ([1], [], fromList . map read $ splitOn "," s, 0)

day05a :: String -> String
day05a s = let (_, output, _, _) = execState run (initial s) in
  show $ head output

day05b :: String -> String
day05b = undefined
