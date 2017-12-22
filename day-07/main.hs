#!/bin/env stack
-- stack runghc --package containers
{-# LANGUAGE ViewPatterns, LambdaCase #-}

import System.Environment (getArgs)

import Data.List

import Tree

type Program = String
type Weight = Int

getInput :: IO [NodeInfo Program Weight]
getInput = getArgs >>= \case
        n:_ -> fromFile n
        _ -> fromFile "input.txt"
    

fromFile :: FilePath -> IO [NodeInfo Program Weight]
fromFile fname = map parseInfo . lines <$> readFile fname

main = do
    tree <- nodeInfoToTree  <$> getInput
    let Node root _ = unfix tree
    putStrLn $ "Root node: " ++ root
    putStrLn $ case balance tree of
        Nothing -> "Tree is already balanced!"
        Just (node, weight) -> "Change weight of node " ++ node ++ " to " ++ show weight


parseInfo :: String -> NodeInfo Program Weight
parseInfo (words -> name:weight':rest) = TreeF name (readWeight weight') $ case rest of
    "->":others -> map (filter (/= ',')) others
    _ -> []
  where
    readWeight = read . filter (not . (`elem` "()"))


