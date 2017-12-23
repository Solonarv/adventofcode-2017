{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day2.Main where

import Data.List (foldl')

import Harness

instance Solution 2 where
    type Input 2 = [[Int]]
    type Output 2 = Int
    readInput = simpleReadInput $ map (map read . words) . lines
    process _ = sum . map checksumRow
    
checksumRow :: [Int] -> Int
checksumRow = uncurry (-) . foldl' go (minBound, maxBound)
  where
    go (hi, lo) x = (max hi x, min lo x)