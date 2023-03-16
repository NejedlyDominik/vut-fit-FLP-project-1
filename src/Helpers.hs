-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

module Helpers (
    errorExit
) where

errorExit :: String -> IO a
errorExit = ioError . userError
