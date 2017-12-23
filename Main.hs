{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
module Main where

import qualified Day1.Main as Day01
import qualified Day2.Main as Day02
import qualified Day3.Main as Day03
import qualified Day4.Main as Day04
import qualified Day5.Main as Day05
import qualified Day6.Main as Day06
import qualified Day7.Main as Day07

import Control.Monad

import Data.Singletons
import Data.Singletons.TypeLits

import Harness
import HarnessTH

solutionRunners :: [(Integer, SolutionRunner)]
solutionRunners = $collectSolutions

main = mapM_ runSimply solutionRunners

runSimply :: (Integer, SolutionRunner) -> IO ()
runSimply (i, runner) = do
    putStrLn $ "\nRunning day " ++ show i ++ " on default file:"
    runner Nothing