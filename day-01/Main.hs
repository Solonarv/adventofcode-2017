#!/bin/env stack
-- stack runghc


getInput :: IO [Int]
getInput = map (read . (:[])) . head . lines <$> readFile "input.txt"

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits ds@(h:_) = go h 0 ds
  where 
    go hd acc (x:y:xs) = if x == y then go hd (acc + x) (y:xs) else go hd acc (y:xs)
    go hd acc (x:[]) = if hd == x then (acc + x) else acc

main = sumDigits <$> getInput >>= print