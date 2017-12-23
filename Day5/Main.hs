{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day5.Main where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Harness

instance Solution 5 where
    type Input 5 = Vector Int
    type Output 5 = Int
    readInput = simpleReadInput $ V.fromList . map read . lines
    process _ = stepsToExit

stepsToExit :: Vector Int -> Int
stepsToExit = go 0 0
  where
    go stepsTaken location instructions
        | location < 0 || location >= V.length instructions = stepsTaken
        | otherwise = let
            jmp = instructions V.! location
            newLocation = location + jmp
          in go (stepsTaken + 1) (newLocation) (instructions V.// [(location, jmp + 1)])
