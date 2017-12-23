{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day1.Main where

import Harness

instance Solution 1 where
    type Input 1 = [Int]
    type Output 1 = Int
    readInput = simpleReadInput $ map (read . (:[])) . head . lines
    process _ = sumDigits

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits ds@(h:_) = go h 0 ds
  where 
    go hd acc (x:y:xs) = if x == y then go hd (acc + x) (y:xs) else go hd acc (y:xs)
    go hd acc (x:[]) = if hd == x then (acc + x) else acc