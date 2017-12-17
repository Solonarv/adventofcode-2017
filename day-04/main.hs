#!/bin/env stack
-- stack runghc

getInput :: IO [[String]]
getInput = map words . lines <$> readFile "input.txt"

