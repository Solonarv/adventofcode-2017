{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day4.Main where

import qualified Data.Set as S
import Data.List (foldl')
import Data.Maybe

import Harness

instance Solution 4 where
    type Input 4 = [[String]]
    type Output 4 = Int
    readInput = simpleReadInput $ map words . lines
    process _ = countValid


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