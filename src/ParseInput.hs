-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

-- Module providing input parsing

module ParseInput (
    parseInput
) where

import Text.Parsec (ParseError, char, digit, spaces, string, many1, sepEndBy, parse, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Perm ((<$$>), (<||>), permute)

-- Import due to compatibily with merlin haskell version
import Control.Applicative ((<$>), (<*), (*>))

import Types (Item(..), Knapsack(..))

-- Parse knapsack instance in the input format into its internal representation.
parseInput :: String -> String -> Either ParseError Knapsack
parseInput = parse knapsack

-- Parser providing parsing of a knapsack instance in the input format into its internal representation
knapsack :: Parser Knapsack
knapsack = do
    _ <- spaces *> string "Knapsack" *> spaces *> char '{'
    (mW, mC, is) <- permute ((,,)
        <$$> field "maxWeight" int
        <||> field "minCost" int
        <||> field "items" itemList)
    _ <- spaces <* char '}'
    return $ Knapsack {maxWeight = mW, minCost = mC, items = is}

-- Parser providing parsing of a knapsack/item field into its internal representation
field :: String -> Parser a -> Parser a
field name parser = try (spaces *> string name) *> spaces *> char ':' *> spaces *> parser <* spaces

-- Parser providing parsing of a non-negative integer value into its internal representation
int :: Parser Int
int = read <$> many1 digit

-- Parser providing parsing of the list of items in the input format into its internal representation
itemList :: Parser [Item]
itemList = char '[' *> spaces *> sepEndBy item spaces <* char ']'

-- Parser providing parsing of an item in the input format into its internal representation
item :: Parser Item
item = do
    _ <- string "Item" *> spaces *> char '{'
    (w, c) <- permute ((,)
        <$$> field "weight" int
        <||> field "cost" int)
    _ <- char '}'
    return $ Item {weight = w, cost = c}
