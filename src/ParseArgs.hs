-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

module ParseArgs (
    parseArgs,
    iOpt,
    bOpt,
    oOpt
) where

import System.Console.GetOpt (getOpt, usageInfo, ArgDescr(NoArg), ArgOrder(Permute), OptDescr(..))

import Helpers (errorExit)

data Options = Options {
    iOpt :: Bool,
    bOpt :: Bool,
    oOpt :: Bool
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    iOpt = False,
    bOpt = False,
    oOpt = False
}

options :: [OptDescr (Options -> Options)]
options = [
    Option ['i'] [] (NoArg (\opts -> opts {iOpt = True}))
        "print the input knapsack instance to stdout (via Show type class)",
    Option ['b'] [] (NoArg (\opts -> opts {bOpt = True}))
        "print the solution of the input Knapsack instance found by brute\
        \ force search of the state space (False if not found)",
    Option ['o'] [] (NoArg (\opts -> opts {oOpt = True}))
        "pprint the solution of the input Knapsack instance found by genetic\
        \ algorithm (False if not found)"
    ]

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv =
    case getOpt Permute options argv of
        ([], _, []) -> errorExit $ "no option specified\n" ++ usageInfo header options
        (opts, nonOpts, []) -> return (foldl (flip id) defaultOptions opts, nonOpts)
        (_, _, errs) -> errorExit $ concat errs ++ usageInfo header options
  where
    header = "Usage: flp22-fun OPTION [OPTION...] [INPUT] [OPTION...]"
