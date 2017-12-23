{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day6.Main where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

import Harness

instance Solution 6 where
    type Input 6 = Vector Int
    type Output 6 = Cycle
    readInput = simpleReadInput $ V.fromList . map read . words . head . lines
    process _ = findCycle stepMemoryBanks

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

findCycle :: Ord a => (a -> a) -> a -> Cycle
findCycle step start = go 1 (M.singleton start 1) start
  where
    go n seen (step -> next) = case M.lookup next seen of
        Just lastSeen -> Cycle n (n - lastSeen)
        Nothing ->  go (succ n) (M.insert next n seen) next

data Cycle = Cycle {
    firstRepeatingValue :: Int,
    cycleLength :: Int
    }

instance Show Cycle where
    show (Cycle rep len) = "Cycle of length " ++ show len ++ " ending at index " ++ show rep