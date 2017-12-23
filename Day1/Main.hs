{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day1.Main where

import Control.Arrow ((&&&))
import qualified Data.Vector as V

import Harness

instance Solution 1 where
    type Input 1 = [Int]
    type Output 1 = (Int, Int)
    readInput = simpleReadInput $ map (read . (:[])) . head . lines
    process _ = sumNext &&& sumOpposite
    displayOutput _ (nextSum, oppositeSum) = do
        putStr "Sum of digits that match the next digit:"
        print nextSum
        putStr "Sum of digits that match the opposite digit:"
        print oppositeSum

sumNext :: [Int] -> Int
sumNext [] = 0
sumNext ds@(h:_) = go h 0 ds
  where 
    go hd acc (x:y:xs) = if x == y then go hd (acc + x) (y:xs) else go hd acc (y:xs)
    go hd acc (x:[]) = if hd == x then (acc + x) else acc

sumOpposite :: [Int] -> Int
sumOpposite xs = V.sum $ V.zipWith addIfEqual hi lo
  where
    vec = V.fromList xs
    (lo, hi) = V.splitAt (V.length vec `div` 2) vec
    addIfEqual x y
        | x == y    = x + y
        | otherwise = 0