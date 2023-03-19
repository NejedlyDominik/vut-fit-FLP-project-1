-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

-- Module providing an error termination function

module Helpers (
    errorExit
) where

-- Raise an IOException with a given error message (i.e. terminate code execution).
errorExit :: String -> IO a
errorExit = ioError . userError
