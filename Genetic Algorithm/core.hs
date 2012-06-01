import System.Random

-- This is the core set of functions necessary for a genetic  algorithm
-- These will be rather rigid to begin with, representing the cannonical
-- algorithm and be more open in later versions.


-- Represents a way to encode information in a binary form by
-- producing random candidate solutions.  Binary String represented as
-- a list of boolean values
type Chromosome = [Bool]
type Mutation = Chromosome -> Chromosome
type FitnessFunction = Chromosome -> Int
type Encoding = Int

data Encoding encoding = 100

getNewSolution :: [] -> Chromosome


-- RANDOM NUMBER GENERATOR
type Rand a = StdGen -> (a, StdGen)

getPRNG = do
    rng <- newStdGen
    let x = usePRNG rng
    print x

usePRNG :: StdGen -> [[Int]]
usePRNG rng = let (x, rng') = randomInts 5 rng
                  (y, _) = randomInts 10 rng'
              in [x, y]

randomInts :: Int -> Rand [Int]
randomInts 0 rng = ([], rng)
randomInts n rng = let (x, rng') = next rng
                       (xs, rng'') = randomInts (n - 1) rng'
                   in (x:xs, rng'')
-- RANDOM NUMBER GENERATOR


-- Highest level function takes an encoding, a fitness function,
-- a list of mutation functions and returns a solution (chromosome)
genAlg :: Encoding -> [Mutation] -> FitnessFunction -> Chromosome


-- 
