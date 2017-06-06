module Main where

import Data.Search.Forward.AStar
import Data.Search.Forward.RBFS
import Data.Search.Forward.NonOptimal
import Criterion.Main

main :: IO ()
main = defaultMain 
    [ bgroup "astar"
        [ bgroup "trivial" 
            [ bench "trivial 100" $ nf (astar1D trivial) 100
            , bench "trivial 1000" $ nf (astar1D trivial) 1000
            , bench "trivial 10000" $ nf (astar1D trivial) 10000
            , bench "trivial 100000" $ nf (astar1D trivial) 100000
            ]
        , bgroup "branched" 
            [ bench "branched 100" $ nf (astar1D branched) 100
            , bench "branched 1000" $ nf (astar1D branched) 1000
            , bench "branched 10000" $ nf (astar1D branched) 10000
            , bench "branched 100000" $ nf (astar1D branched) 100000
            ]
        , bgroup "looped" 
            [ bench "looped 10000" $ nf (astar1D looped) 10000
            , bench "looped 100000" $ nf (astar1D looped) 100000
            ]
        , bgroup "looped0"
            [ bench "looped0 10000" $ nf (astar1D looped0) 10000
            , bench "looped0 100000" $ nf (astar1D looped0) 100000
            ]
        ]
    , bgroup "idastar"
        [ bgroup "trivial" 
            [ bench "trivial 100" $ nf (idastar1D trivial) 100
            , bench "trivial 1000" $ nf (idastar1D trivial) 1000
            , bench "trivial 10000" $ nf (idastar1D trivial) 10000
            , bench "trivial 100000" $ nf (idastar1D trivial) 100000
            ]
        , bgroup "branched" 
            [ bench "branched 100" $ nf (idastar1D branched) 100
            , bench "branched 1000" $ nf (idastar1D branched) 1000
            , bench "branched 10000" $ nf (idastar1D branched) 10000
            , bench "branched 100000" $ nf (idastar1D branched) 100000
            ]
        , bgroup "looped" 
            [ bench "looped 10000" $ nf (idastar1D looped) 10000
            , bench "looped 100000" $ nf (idastar1D looped) 100000
            ]
        ]
    , bgroup "rbfs"
        [ bgroup "trivial" 
            [ bench "trivial 100" $ nf (rbfs1D trivial) 100
            , bench "trivial 1000" $ nf (rbfs1D trivial) 1000
            , bench "trivial 10000" $ nf (rbfs1D trivial) 10000
            , bench "trivial 100000" $ nf (rbfs1D trivial) 100000
            ]
        , bgroup "branched" 
            [ bench "branched 100" $ nf (rbfs1D branched) 100
            , bench "branched 1000" $ nf (rbfs1D branched) 1000
            , bench "branched 10000" $ nf (rbfs1D branched) 10000
            , bench "branched 100000" $ nf (rbfs1D branched) 100000
            ]
        , bgroup "looped" 
            [ bench "looped 10000" $ nf (rbfs1D looped) 10000
            , bench "looped 100000" $ nf (rbfs1D looped) 100000
            ]
        ]
    , bgroup "non-optimal"
        [ bench "dls (250000, 18)" $ nf binaryDLS (250000, 18)
        , bench "bfs (4000, 12)" $ nf binaryBFS (4000, 12)
        , bench "ids (250000, 18)" $ nf binaryIDS (250000, 18)
        ]
    ]

euclidian1D goal x = abs $ goal - x
trivial x = [(x + 1, 1)]
branched x = [(x + 1, 1), (x - 1, 1)]
looped x = [(x + 1, 1), (x, 1)]
looped0 x = [(x + 1, 1), (x, 0)]

astar1D :: (Int -> [(Int, Int)]) -> Int -> Maybe [Int]
astar1D neighbor goal = astar' neighbor (euclidian1D goal) (== goal) 0

idastar1D :: (Int -> [(Int, Int)]) -> Int -> Maybe [Int]
idastar1D neighbor goal = idastar' neighbor (euclidian1D goal) (== goal) 0

rbfs1D :: (Int -> [(Int, Int)]) -> Int -> Maybe [Int]
rbfs1D neighbor goal = rbfs' neighbor (euclidian1D goal) (== goal) 0

binaryTree :: (Int, Int) -> [((Int, Int), Char)]
binaryTree (k,l) = let l' = succ l in [((2*k - 1, l'), 'l'), ((2*k, l'), 'r')]

binaryBFS :: (Int, Int) -> Maybe [Char]
binaryBFS g = bfs binaryTree (== g) (1,0)

binaryDLS :: (Int, Int) -> Maybe [Char]
binaryDLS g = dls 20 binaryTree (== g) (1,0)

binaryIDS :: (Int, Int) -> Maybe [Char]
binaryIDS g = ids Nothing binaryTree (== g) (1,0)
