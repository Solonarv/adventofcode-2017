#!/bin/env stack
-- stack runghc --package containers
{-# LANGUAGE ViewPatterns, LambdaCase #-}

import System.Environment (getArgs)

import Control.Monad.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Monoid hiding ((<>))
import Data.Semigroup hiding (First(..))
import Data.List

getInput :: IO [ProgramInfo]
getInput = getArgs >>= \case
        n:_ -> fromFile n
        _ -> fromFile "input.txt"
    

fromFile :: FilePath -> IO [ProgramInfo]
fromFile fname = map parseInfo . lines <$> readFile fname

main = do
    tree <- foldMap infoToMaps <$> getInput
    let root = rootNode (parents tree)
    putStrLn $ "Root node: " ++ root
    putStrLn $ case balance tree root of
        Nothing -> "Tree is already balanced!"
        Just (node, weight) -> "Change weight of node " ++ node ++ " to " ++ show weight

data ProgramInfo = ProgramInfo String Int [String] deriving (Eq, Ord, Show)

parseInfo :: String -> ProgramInfo
parseInfo (words -> name:weight':rest) = ProgramInfo name (readWeight weight') $ case rest of
    "->":others -> map (filter (/= ',')) others
    _ -> []
  where
    readWeight = read . filter (not . (`elem` "()"))

data Graph a = Graph {
    parents :: Map a a,
    children :: Map a [a],
    weights :: Map a Int
    } deriving (Eq, Show)

instance Ord a => Semigroup (Graph a) where
    Graph lp lc lw <> Graph rp rc rw
        = Graph (lp <> rp) (M.unionWith (++) lc rc) (lw <> rw)
instance Ord a => Monoid (Graph a) where
    mempty = Graph mempty mempty mempty
    mappend = (<>)

infoToMaps :: ProgramInfo -> Graph String
infoToMaps (ProgramInfo parent weight children) = Graph
    (M.fromList [(child, parent) | child <- children])
    (M.singleton parent children)
    (M.singleton parent weight)

-- | Find a root node for the given graph. 
-- If there is no unique root node, one of the root nodes
-- will be chosen arbitrarily. Will error if given an empty graph.
rootNode :: Ord a => Map a a -> a
rootNode pars 
  | M.null pars = error "rootNode: empty graph"
  | otherwise = recursiveRoot pars start
  where
    start:_ = M.keys pars

-- | Find the root node starting from a given node.
-- WARNING: if a cycle is encountered, this function will hang indefinitely.
recursiveRoot :: Ord a => Map a a -> a -> a
recursiveRoot pars s = case M.lookup s pars of
    Nothing -> s
    Just s' -> recursiveRoot pars s'

balance :: Ord a => Graph a -> a -> Maybe (a, Int)
balance g root = let
    immediateChildren = M.findWithDefault [] root (children g)
    childWeights = zip immediateChildren $ map (totalWeight g) immediateChildren
    nodesByWeight = foldl' (\m (n,w) -> M.insertWith (++) w [n] m) M.empty childWeights
    isBalanced = M.size nodesByWeight <= 1
    -- This value is only used if the list is in fact non-empty, so head is safe.
    oddOneOut = head . filter ((== 1) . length . snd) $ M.assocs nodesByWeight
    mainWeight = fst . head . filter (/= oddOneOut) $ M.assocs nodesByWeight
        
  in if isBalanced
    then getFirst . foldMap First $ map (balance g) immediateChildren
    else Just (head $ snd oddOneOut, mainWeight)

totalWeight :: Ord a => Graph a -> a -> Int
totalWeight g n = let
    cs = M.findWithDefault [] n (children g)
  in (weights g) M.! n + sum (map (totalWeight g) cs)