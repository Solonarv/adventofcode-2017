{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day2.Main where

import Data.List (foldl')
import Data.Monoid
import Data.Bifunctor

import qualified Data.Set as S

import Control.Arrow ((&&&))

import Harness

instance Solution 2 where
    type Input 2 = [[Int]]
    type Output 2 = (Int, Int)
    readInput = simpleReadInput $ map (map read . words) . lines
    process _ = both getSum . foldMap (both Sum) . map (checksumRow &&& divsumRow)
    displayOutput _ (checksum, divsum) = do
        putStr "The checksum is "
        print checksum
        putStr "The sum of divisions is "
        print divsum
    
checksumRow :: [Int] -> Int
checksumRow = uncurry (-) . foldl' go (minBound, maxBound)
  where
    go (hi, lo) x = (max hi x, min lo x)

divsumRow :: [Int] -> Int
divsumRow row = case foldl' go (Left S.empty) row of
    Left _ -> error "divsumRow: no two values evenly divide each other"
    Right q -> q
  where
    go :: Either (S.Set Int) Int -> Int -> Either (S.Set Int) Int
    go (Right q) _ = Right q
    go (Left seen) x = let
        divs = S.filter ((==0) . snd)
            $ S.map (`quotRem` x) seen
            <> S.map (x `quotRem`) seen
      in case S.minView divs of
        Nothing -> Left (S.insert x seen)
        Just ((q, _), _) -> Right q

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f