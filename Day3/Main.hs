{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day3.Main where

import Control.Monad (join)
import Control.Arrow ((&&&))

import Harness

instance Solution 3 where
    type Input 3 = Int
    type Output 3 = Int
    process _ = stepsToCenter

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