module Main where

import Data.Search.Forward.AStar

euclidian1D goal x = abs $ goal - x
trivial x = [(x + 1, 1)]

astar1D :: (Int -> [(Int, Int)]) -> Int -> Maybe [Int]
astar1D neighbor goal = astar' neighbor (euclidian1D goal) (== goal) 0

main :: IO ()
main = do
    let x = astar1D trivial 1000000
    print (last <$> x)
