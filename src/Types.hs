-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

-- Module containing data type definitions for internal representation of the knapsack, its parts and its solution

module Types (
    Knapsack(..),
    Item(..),
    Solution(..),
    Cost,
    Weight,
    Flag,
    RatedFlags
) where

type Cost = Int
type Weight = Int

type Flag = Bool
type RatedFlags = ([Flag], Cost)

data Knapsack = Knapsack {
    maxWeight :: Weight,
    minCost :: Cost,
    items :: [Item]
}

-- Show internal representation of knapsack instance in predefined (input) format.
instance Show Knapsack where
    show (Knapsack {maxWeight = mW, minCost = mC, items = is}) =
        "Knapsack {\nmaxWeight: " ++ show mW ++ "\nminCost: " ++ show mC
        ++ "\nitems: [" ++ formatItems is ++ "]\n}"
      where
        formatItems [] = ""
        formatItems xs = concatMap (concatMap ("\n    "++) . lines . show) xs ++ "\n"

data Item = Item {
    weight :: Weight,
    cost :: Cost
}

-- Show internal representation of item instance in predefined (input) format.
instance Show Item where
    show (Item {weight = w, cost = c}) = "Item {\nweight: "
        ++ show w ++ "\ncost: " ++ show c ++ "\n}"

newtype Solution = Solution {
    -- Flags indicating selected items (True <=> selected)
    flags :: [Flag]
}

-- Show internal representation of solution instance in predefined expected
-- format (i.e. Solution [1 0 1 ...]).
instance Show Solution where
    show Solution {flags = fs} = "Solution [" ++ foldr formatFlags [] fs
      where
        formatFlags True [] = "1]"
        formatFlags True acc = '1' : ' ' : acc
        formatFlags False [] = "0]"
        formatFlags False acc = '0' : ' ' : acc
