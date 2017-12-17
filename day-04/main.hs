#!/bin/env stack
-- stack runghc

import qualified Data.Set as S
import Data.List (foldl')
import Data.Maybe

main = countValid <$> getInput >>= print

getInput :: IO [[String]]
getInput = map words . lines <$> readFile "input.txt"

countValid :: Ord a => [[a]] -> Int
countValid = countTrues . map isValid

countTrues :: [Bool] -> Int
countTrues = foldl' go 0
  where
    go n True = n + 1
    go n False = n
    
isValid :: Ord a => [a] -> Bool
isValid = not . containsDuplicates

containsDuplicates :: Ord a => [a] -> Bool
containsDuplicates = isNothing . foldr go (Just S.empty)
  where
    go _ Nothing = Nothing
    go word (Just seen) = if S.member word seen
        then Nothing
        else Just (S.insert word seen)