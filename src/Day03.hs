module Day03(day03a, day03b) where

import Data.Function

import Day03Impl

day03a :: String -> String
day03a input = show $ (distFromClosestMan `on` parseLine) l1 l2
  where (l1:l2:_) = lines input

day03b :: String -> String
day03b input = show $ (distFromClosestSignal `on` parseLine) l1 l2
  where (l1:l2:_) = lines input
