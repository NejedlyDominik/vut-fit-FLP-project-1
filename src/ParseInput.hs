-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

module ParseInput (
    parseInput
) where

import Text.Parsec (ParseError, char, digit, spaces, string, many1, sepEndBy, parse, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Perm ((<$$>), (<||>), permute)

-- Import due to compatibily with merlin haskell version
import Control.Applicative ((<$>), (<*), (*>))

import Types (Item(..), Knapsack(..))

parseInput :: String -> String -> Either ParseError Knapsack
parseInput = parse knapsack

knapsack :: Parser Knapsack
knapsack = do
    _ <- spaces *> string "Knapsack" *> spaces *> char '{'
    (mW, mC, is) <- permute ((,,)
        <$$> field "maxWeight" int
        <||> field "minCost" int
        <||> field "items" itemList)
    _ <- spaces <* char '}'
    return $ Knapsack {maxWeight = mW, minCost = mC, items = is}

field :: String -> Parser a -> Parser a
field name parser = try (spaces *> string name) *> spaces *> char ':' *> spaces *> parser <* spaces

int :: Parser Int
int = read <$> many1 digit

itemList :: Parser [Item]
itemList = char '[' *> spaces *> sepEndBy item spaces <* char ']'

item :: Parser Item
item = do
    _ <- string "Item" *> spaces *> char '{'
    (w, c) <- permute ((,)
        <$$> field "weight" int
        <||> field "cost" int)
    _ <- char '}'
    return $ Item {weight = w, cost = c}
