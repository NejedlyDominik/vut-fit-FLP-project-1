-- Functional project - Knapsack problem (version 0-1)

-- login: xnejed09
-- name: Dominik Nejedlý
-- year: 2023

-- Module providing a solution to the 0-1 knapsack problem using a genetic algorithm

module GeneticAlg (
    search
) where

import Data.Foldable (maximumBy)
import System.Random (RandomGen, random, randomR)
import qualified Data.Vector as V (Vector, fromList, (!))

import Types (Knapsack(..), Item(..), Solution(..), RatedFlags, Weight, Cost, Flag)

-- Predefined parameters

populationSize :: Int
populationSize = 400

numOfGenerations :: Int
numOfGenerations = 500

crossoverRate :: Double
crossoverRate = 0.4

reproductionRate :: Double
reproductionRate = 0.3

mutationRate :: Double
mutationRate = 0.05

-- Generate random rate (number from interval <0, 1>)
randomRate :: (RandomGen g) => g -> (Double, g)
randomRate = random

-- Try to find solution to the given knapsack instance. Return Just Solution or Nothing
-- in case there is no solution.
search :: (RandomGen g) => Knapsack -> g -> Maybe Solution
search (Knapsack {maxWeight = mW, minCost = mC, items = is}) gen
    | totalCost < mC = Nothing
    | otherwise = Just Solution {flags = fs}
  where
    (fs, totalCost) = geneticAlgorithm mW is gen

-- Run the genetic algorithm with predefined parameters.
geneticAlgorithm :: (RandomGen g) => Weight -> [Item] -> g -> RatedFlags
geneticAlgorithm mW is gen =
    let
        solLen = length is
        crossPoint = round $ crossoverRate * fromIntegral solLen
        (initPop, newGen) = generatePopulation mW is populationSize solLen gen
    in
        maximumBy compareSol $ runEvolution mW is initPop populationSize numOfGenerations crossPoint newGen
  where
    compareSol (_, totalCost1) (_, totalCost2) = compare totalCost1 totalCost2

-- Generate initial population.
generatePopulation :: (RandomGen g) => Weight -> [Item] -> Int -> Int -> g -> ([RatedFlags], g)
generatePopulation _ _ 0 _ gen = ([], gen)
generatePopulation mW is popSize solLen gen = ((member, totalCost) : restOfPopulation, finalGen)
  where
    (member, newGen) = randomSolution solLen gen
    totalCost = fitness mW is member
    (restOfPopulation, finalGen) = generatePopulation mW is (popSize - 1) solLen $! newGen

-- Generate random flags that represent potential solution.
randomSolution :: (RandomGen g) => Int -> g -> ([Flag], g)
randomSolution 0 gen = ([], gen)
randomSolution len gen = (flag : restOfSolution, finalGen)
  where
    (flag, newGen) = random gen
    (restOfSolution, finalGen) = randomSolution (len - 1) newGen

-- Run the evolution with the predefined parameters for the given initial population.
runEvolution :: (RandomGen g) => Weight -> [Item] -> [RatedFlags] -> Int -> Int -> Int -> g -> [RatedFlags]
runEvolution _ _ population _ 0 _ _ = population
runEvolution mW is population popSize numOfGens crossPoint gen =
    runEvolution mW is nextPop popSize (numOfGens - 1) crossPoint $! newGen
  where
    (nextPop, newGen) = nextGeneration mW is (V.fromList population) popSize crossPoint gen

-- Create next generation
nextGeneration :: (RandomGen g) => Weight -> [Item] -> V.Vector RatedFlags -> Int -> Int -> g -> ([RatedFlags], g)
nextGeneration mW is population popSize crossPoint gen
    | popSize <= 0 = ([], gen)
    | otherwise = (child1 : child2 : restOfNextPop, finalGen)
  where
    (child1, child2, newGen) = getChildren mW is population crossPoint gen
    (restOfNextPop, finalGen) = nextGeneration mW is population (popSize - 2) crossPoint $! newGen

-- Select parents and based on randomness and reproduction rate return either them or their children.
getChildren :: (RandomGen g) => Weight -> [Item] -> V.Vector RatedFlags -> Int -> g -> (RatedFlags, RatedFlags, g)
getChildren mW is population crossPoint gen
    | rate < reproductionRate = (parent1, parent2, newGen3)
    | otherwise =
        let
            (unmutChildFs1, unmutChildFs2) = crossover parentFs1 parentFs2 crossPoint
            (childFs1, newGen4) = mutate unmutChildFs1 newGen3
            (childFs2, finalGen) = mutate unmutChildFs2 newGen4
        in
            ((childFs1, fitness mW is childFs1), (childFs2, fitness mW is childFs2), finalGen)
  where
    (rate, newGen1) = randomRate gen
    (parent1@(parentFs1, _), newGen2) = getParent population newGen1
    (parent2@(parentFs2, _), newGen3) = getParent population newGen2

-- Create two children by crossing their parents.
crossover :: [Flag] -> [Flag] -> Int -> ([Flag], [Flag])
crossover parentFs1 parentFs2 crossPoint =
    let
        (parentFs11, parentFs12) = splitAt crossPoint parentFs1
        (parentFs21, parentFs22) = splitAt crossPoint parentFs2
    in
        (parentFs11 ++ parentFs22, parentFs21 ++ parentFs12)

-- Mutate child.
mutate :: (RandomGen g) => [Flag] -> g -> ([Flag], g)
mutate sol gen = foldr mutateFlag ([], gen) sol
  where
    mutateFlag f (fs, gen')
        | rate < mutationRate = (not f : fs, newGen)
        | otherwise = (f : fs, newGen)
      where
        (rate, newGen) = randomRate gen'

-- Select parent by tournament.
getParent :: (RandomGen g) => V.Vector RatedFlags -> g -> (RatedFlags, g)
getParent population gen =
    let
        (idx1, newGen) = randomR (0, populationSize - 1) gen
        (idx2, finalGen) = randomR (0, populationSize - 1) newGen
        sol1@(_, totalCost1) = population V.! idx1
        sol2@(_, totalCost2) = population V.! idx2
    in
        if totalCost1 > totalCost2 then (sol1, finalGen) else (sol2, finalGen)

-- Score the given potential solution based on its total cost
-- (0 if the minimum cost is not met).
fitness :: Weight -> [Item] -> [Flag] -> Cost
fitness mW is fs
    | tW > mW = 0
    | otherwise = tC
  where
    filteredItems =  map fst . filter snd $ zip is fs
    tW = sum $ map weight filteredItems
    tC = sum $ map cost filteredItems
