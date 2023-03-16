-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

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

instance Show Item where
    show (Item {weight = w, cost = c}) = "Item {\nweight: "
        ++ show w ++ "\ncost: " ++ show c ++ "\n}"

newtype Solution = Solution {
    flags :: [Flag]
}

instance Show Solution where
    show Solution {flags = fs} = "Solution [" ++ foldr formatFlags [] fs
      where
        formatFlags True [] = "1]"
        formatFlags True acc = '1' : ' ' : acc
        formatFlags False [] = "0]"
        formatFlags False acc = '0' : ' ' : acc
