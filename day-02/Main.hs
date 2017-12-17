#!/bin/env stack
-- stack runghc


import Data.List (foldl')

checksumRow :: [Int] -> Int
checksumRow = uncurry (-) . foldl' go (minBound, maxBound)
  where
    go (hi, lo) x = (max hi x, min lo x)

getInput :: IO [[Int]]
getInput = map (map read . words) . lines <$> readFile "input.txt"

main = sum . map checksumRow <$> getInput >>= print