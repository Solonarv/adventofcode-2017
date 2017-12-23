{-# LANGUAGE
    TemplateHaskell
    #-}
module HarnessTH (collectSolutions) where

import Language.Haskell.TH
import Data.Proxy
import Data.List

import System.IO

import Harness

-- | Collect all instances of @Solution@ in the current scope, generate a @SolutionRunner@ for each of them,
-- and return the list containing all those runners.
collectSolutions :: Q Exp
collectSolutions = do
    ClassI _ instances <- reify ''Solution
    listE . map mkRunner . sortOn tyNum . map instTy $ instances 
  where
    instTy (InstanceD _ _ (AppT _ ty) _) = ty
    tyNum (LitT (NumTyLit n)) = n
    mkRunner ty = let n = tyNum ty in [e| (n, runSolution' $(mkProxy ty)) |]
    mkProxy ty = [e| Proxy :: Proxy $(pure ty) |]