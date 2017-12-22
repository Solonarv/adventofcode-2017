{-# LANGUAGE
    DeriveFunctor,
    ScopedTypeVariables,
    PatternSynonyms,
    ViewPatterns,
    FlexibleContexts
    #-}
module Tree (
    module Tree,
    unfix
    ) where

import Data.Functor.Foldable
import Data.Functor.Classes

import qualified Data.Map as M
import Data.List
import Data.Either
import Data.Maybe
import Data.Monoid

data TreeF i ann a = TreeF i ann [a]
    deriving (Eq, Ord, Show, Functor)

instance (Eq i, Eq ann) => Eq1 (TreeF i ann) where
    liftEq eq (TreeF j a xs) (TreeF k b ys) = j == k && a == b && go xs ys
      where
        go (x:xs) (y:ys) = eq x y
        go [] [] = True
        go _ _ = False

type Tree i ann = Fix (TreeF i ann)

type Node i ann = TreeF ()

pattern Node i a <- TreeF i a _
  where Node i a = TreeF i a []

type NodeInfo i ann = TreeF i ann i

nodeInfoToTree :: (Foldable t, Ord i) => t (NodeInfo i ann) -> Tree i ann
nodeInfoToTree infos = ana findChildren root
  where
    infoMap = foldMap infoToEntry infos
    infoToEntry (TreeF p a cn) = M.singleton p (a, cn)
    root = assocFindRoot (snd <$> infoMap)
    findChildren node = case M.lookup node infoMap of
        Nothing -> error "node not present in graph data"
        Just (w, cn) -> TreeF node w cn

assocFindRoot :: Ord i => M.Map i [i] -> i
assocFindRoot p2c = if M.null c2p
    then error "assocFindRoot: empty map"
    else go someNode
  where
    p2cAssocs = M.assocs p2c
    c2p = M.fromList [ (child, parent) | (parent, children) <- p2cAssocs, child <- children ]
    someNode = fst $ head p2cAssocs
    go cur = case M.lookup cur c2p of
        Nothing -> cur
        Just par -> go par

-- | A suitable argument for @para@ specialized to @Fix@.
type ParaAlgebra f a = f (Fix f, a) -> a

balance :: (Num ann, Ord ann, Ord i) => Tree i ann -> Maybe (i, ann)
balance = eitherToMaybe . para go
  where
    -- go :: ParaAlgebra (TreeF i ann) (Either ann (i, ann))
    go (TreeF i a cn) =
      let
        (okay, altered) = partitionEithers . map snd $ cn
      in case altered of
        x:_ -> Right x
        [] -> case oddOneOut cn of
            Nothing -> Left (sum okay + a)
            Just (mainWeight, (Node i wrongLocal, Left wrongTotal)) -> Right (i, wrongLocal + mainWeight - wrongTotal)
    getAnn (Node _ ann) = ann
    --oddOneOut :: (Num ann, Eq i) => [(Tree i ann, Either _ _)] -> _
    oddOneOut nodes = let
        grouped = foldl' (\m (unfix -> node, total) -> M.insertWith (++) total [node] m) M.empty nodes
        oddEntryMb = listToMaybe . filter ((==1) . length . snd) . M.assocs $ grouped
        mainWeight oddNode = either id (error "DEATH") .fst . head . filter (not . (oddNode `elem`) . snd) . M.assocs $ grouped
      in case oddEntryMb of
        Just (oddWeight,[oddNode]) -> Just (mainWeight oddNode, (oddNode, oddWeight))
        Nothing -> Nothing
    eitherToMaybe = either (const Nothing) Just