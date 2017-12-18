#!/bin/env stack
-- stack runghc --package vector --package containers
{-# LANGUAGE TupleSections, ViewPatterns #-}

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.Map as M

getInput :: IO (Vector Int)
getInput = V.fromList . map read . words . head . lines <$> readFile "input.txt"

main = getInput >>= print . findCycle stepMemoryBanks

stepMemoryBanks :: Vector Int -> Vector Int
stepMemoryBanks vec = newBank
  where
    len = V.length vec
    focus = V.maxIndex vec
    pages = vec ! focus
    cleared = vec // [(focus, 0)]
    (passes, partial) = pages `quotRem` len
    extraIdxs = (,1) <$> [(focus + offset) `rem` len| offset <- [1..partial]]
    newBank = V.map (+ passes) $ V.accum (+) cleared extraIdxs

findCycle :: Ord a => (a -> a) -> a -> Int
findCycle step start = go 1 (M.singleton start 1) start
  where
    go (succ -> n) cache (step -> next) = case M.lookup next cache of
        Nothing -> go n (M.insert next n cache) next
        Just lastSeen -> n - lastSeen