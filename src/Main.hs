-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

-- Main module containing main function operating with input and output

module Main where

import System.Environment (getArgs)
import Control.Monad (when)
import System.Random (getStdGen)

import ParseArgs (oOpt, iOpt, bOpt, parseArgs)
import ParseInput (parseInput)
import Helpers (errorExit)
import qualified BruteForce as BF (search)
import qualified GeneticAlg as GA (search)

-- Parse input arguments, parse input instance of knapsack from file or standard input
-- and print results according to specified input options to standard output. If no solution
-- is found for the given option '-b' or '-o', its result is False. Results are printed in
-- order '-i', '-b' and '-o'.
main :: IO ()
main = do
    (opts, nonOpts) <- getArgs >>= parseArgs
    let (source, getInput) = checkInput nonOpts
    input <- getInput
    case parseInput source input of
        Left err -> errorExit $ show err
        Right knapsack -> do
            when (iOpt opts) (print knapsack)
            when (bOpt opts) (handleResult $ BF.search knapsack)
            when (oOpt opts) (do
                gen <- getStdGen
                handleResult $ GA.search knapsack gen)
  where
    checkInput [] = ("stdin", getContents)
    checkInput (f:_) = (f, readFile f)
    handleResult = maybe (print False) print
