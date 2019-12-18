module Main where

import Control.Monad
import Text.Printf (printf)

import Cache
import Day01
import Day02
import Day03

solvers :: [[String -> String]]
solvers = [ [day01a, day01b]
          , [day02a, day02b]
          , [day03a, day03b] ]

main :: IO ()
main = do
  putStrLn "(h): cache hit, (m): cache miss"
  forM_ (zip solvers ([1..] :: [Int])) $ \(daily_solvers, day) -> do
    printf "Day %02d\n" day
    input <- readFile $ printf "input/%02d.txt" day
    forM_ (zip daily_solvers ['a'..]) $ \(solver, part) -> do
      (is_cached, solution) <- loadOrCompute day part (solver input)
      printf "  (%c) Part %c: %s\n" (if is_cached then 'h' else 'm') part solution
