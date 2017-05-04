module Main where

import Data.Search.Forward.AStar
import Criterion.Main

main :: IO ()
main = defaultMain 
    [ bgroup "trivial" 
        [ bench "trivial 100" $ nf trivial 100
        , bench "trivial 1000" $ nf trivial 1000
        , bench "trivial 10000" $ nf trivial 10000
        , bench "trivial 100000" $ nf trivial 100000
        ]
    , bgroup "branched" 
        [ bench "branched 100" $ nf branched 100
        , bench "branched 1000" $ nf branched 1000
        , bench "branched 10000" $ nf branched 10000
        , bench "branched 100000" $ nf branched 100000
        ]
    , bgroup "looped" 
        [ bench "looped 10000" $ nf looped 10000
        , bench "looped 100000" $ nf looped 100000
        ]
    , bgroup "looped0"
        [ bench "looped0 10000" $ nf looped0 10000
        , bench "looped0 100000" $ nf looped0 100000
        ]
    ]

trivial :: Int -> Maybe [Int]
trivial goal = astar' 
    (\x -> [(x + 1, 1)])
    (\x -> goal - x)
    (== goal)
    0

branched :: Int -> Maybe [Int]
branched goal = astar'
    (\x -> [(x + 1, 1), (x - 1, 1)])
    (\x -> goal - x)
    (== goal)
    0

looped :: Int -> Maybe [Int]
looped goal = astar'
    (\x -> [(x + 1, 1), (x, 1)])
    (\x -> goal - x)
    (== goal)
    0

looped0 :: Int -> Maybe [Int]
looped0 goal = astar'
    (\x -> [(x + 1, 1), (x, 0)])
    (\x -> goal - x)
    (== goal)
    0
