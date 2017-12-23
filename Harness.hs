{-# LANGUAGE
    DataKinds,
    KindSignatures,
    PolyKinds,
    DefaultSignatures,
    FlexibleInstances,
    TypeFamilies,
    FlexibleContexts,
    ConstraintKinds,
    TypeOperators
    #-}
module Harness where

import System.FilePath
import System.IO

import GHC.TypeLits

type ValidDay n = (KnownNat n, n <= 25, 1 <= n)

class ValidDay day => Solution (day :: Nat) where
    type Input day
    type Output day
    
    readInput :: proxy day -> Handle -> IO (Input day)
    default readInput :: Read (Input day) => proxy day -> Handle -> IO (Input day)
    readInput = simpleReadInput read
    
    process :: proxy day -> Input day -> Output day
    
    displayOutput :: proxy day -> Output day -> IO ()
    default displayOutput :: Show (Output day) => proxy day -> Output day -> IO ()
    displayOutput _ = print
    
    defaultInputFile :: proxy day -> FilePath
    defaultInputFile day = "Day" ++ show (natVal day) </> "input.txt"

simpleReadInput :: (String -> a) -> ignored -> Handle -> IO a
simpleReadInput rd _ h = rd <$> hGetContents h

runSolution :: Solution day => proxy day -> IO ()
runSolution day = runSolution' day Nothing

runSolution' :: Solution day => proxy day -> Maybe FilePath -> IO ()
runSolution' day infile
    = case infile of
        Nothing -> withFile (defaultInputFile day) ReadMode go
        Just "-" -> go stdin
        Just fpath -> withFile fpath ReadMode go
  where
    go handle = do
        input <- readInput day handle
        let output = process day input
        displayOutput day output

type SolutionRunner = Maybe FilePath -> IO ()