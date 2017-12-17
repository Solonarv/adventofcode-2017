#!/bin/env stack
-- stack runghc

import Control.Monad (join)
import Control.Arrow ((&&&))

getInput :: IO Int
getInput = read <$> readFile "input.txt"

findLayer :: Int -> Int
findLayer n = (`quot` 2) . subtract 1 . fst . head $ dropWhile ((< n) . snd) oddSquares

oddSquares :: [(Int, Int)]
oddSquares = map (id &&& square) [1,3..]

layerSize :: Int -> Int
layerSize n = n * 8

layerStart :: Int -> Int
layerStart n = square (n*2 - 1) + 1

square :: Int -> Int
square = join (*)

layerCenters :: Int -> [Int]
layerCenters n = [ start + offset + side*i | i <- [0..3]]
  where
    start = layerStart n
    offset = n - 1
    side = n * 2

stepsToCenter :: Int -> Int
stepsToCenter n = layer + distToSideCenter
  where
    layer = findLayer n
    distToSideCenter = minimum [abs (c - n) | c <- layerCenters layer]