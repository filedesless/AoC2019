module Main where

import Control.Monad(forM_)
import Text.Printf(printf)

import Day01 (day01a, day01b)
import Day02 (day02a, day02b)

solvers :: [[String -> String]]
solvers = [ [day01a, day01b]
          , [day02a, day02b] ]

main :: IO ()
main = do
  forM_ (zip solvers ([1..] :: [Int])) $ \(daily_solvers, day) -> do
    printf "Day %02d\n" day
    input <- readFile $ printf "input/%02d.txt" day
    forM_ (zip daily_solvers ['a'..]) $ \(solver, part) -> do
      printf "  Part %c: %s\n" part $ solver input
