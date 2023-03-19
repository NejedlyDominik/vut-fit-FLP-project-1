-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

module BruteForce (
    search
) where

-- Import due to compatibily with merlin haskell version
import Control.Applicative ((<$>))

import Types (Knapsack(..), Item(..), Solution(..), RatedFlags, Weight, Cost)

search :: Knapsack -> Maybe Solution
search (Knapsack {maxWeight = mW, minCost = mC, items = is}) =
    prepareSolution <$> bruteForce is mW mC undecidedCost
  where
    undecidedCost = sum $ map cost is
    prepareSolution (fs, _) = Solution {flags = fs}

bruteForce :: [Item] -> Weight -> Cost -> Cost -> Maybe RatedFlags
bruteForce is mW mC undecidedCost
    | mW < 0 || mC > undecidedCost = Nothing
    | otherwise = nextItem is mW mC undecidedCost

nextItem :: [Item] -> Weight -> Cost -> Cost -> Maybe RatedFlags
nextItem [] _ mC _ = Just ([], mC)
nextItem ((Item {weight = w, cost = c}):remainingItems) mW mC undecidedCost =
    chooseFlag (bruteForce remainingItems mW mC nextUndecidedCost) $
    bruteForce remainingItems (mW - w) (mC - c) nextUndecidedCost
  where
    nextUndecidedCost = undecidedCost - c

chooseFlag :: Maybe RatedFlags -> Maybe RatedFlags -> Maybe RatedFlags
chooseFlag Nothing Nothing = Nothing
chooseFlag Nothing (Just (fs1, overCost1)) = Just (True : fs1, overCost1)
chooseFlag (Just (fs0, overCost0)) Nothing = Just (False : fs0, overCost0)
chooseFlag (Just (fs0, overCost0)) (Just (fs1, overCost1))
    | overCost0 < overCost1 = Just (False : fs0, overCost0)
    | otherwise = Just (True : fs1, overCost1)
