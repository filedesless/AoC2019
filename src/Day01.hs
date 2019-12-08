module Day01 where

import Day01Impl

day01a :: String -> String
day01a = show . sum . map (fuel . read) . lines

day01b :: String -> String
day01b = show . sum . map (fuel' . read) . lines
