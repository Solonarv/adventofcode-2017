#!/bin/env stack
-- stack runghc --package vector

import Data.Vector (Vector)
import qualified Data.Vector as V

getInput :: IO (Vector Int)
getInput = V.fromList . map read . lines <$> readFile "input.txt"

stepsToExit :: Vector Int -> Int
stepsToExit = go 0 0
  where
    go stepsTaken location instructions
        | location < 0 || location >= V.length instructions = stepsTaken
        | otherwise = let
            jmp = instructions V.! location
            newLocation = location + jmp
          in go (stepsTaken + 1) (newLocation) (instructions V.// [(location, jmp + 1)])

main = stepsToExit <$> getInput >>= print