{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies #-}
module Day7.Main where

import System.Environment (getArgs)

import Data.List

import Day7.Tree

import Harness

instance Solution 7 where
    type Input 7 = [NodeInfo Program Weight]
    type Output 7 = (Program, Maybe (Program, Weight))
    readInput = simpleReadInput $ map parseInfo . lines
    
    process _ (nodeInfoToTree -> tree) = (root, balance tree)
      where Node root _ = unfix tree
    
    displayOutput _ (root, mbChange) = do
        putStrLn $ "Root node: " ++ root
        putStrLn $ case mbChange of
            Nothing -> "Tree is already balanced!"
            Just (node, weight) -> "Change weight of node " ++ node ++ " to " ++ show weight

type Program = String
type Weight = Int


parseInfo :: String -> NodeInfo Program Weight
parseInfo (words -> name:weight':rest) = TreeF name (readWeight weight') $ case rest of
    "->":others -> map (filter (/= ',')) others
    _ -> []
  where
    readWeight = read . filter (not . (`elem` "()"))


