{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Search.Forward.AStar where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashPSQ (HashPSQ)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.HashPSQ as Q

astar :: forall a c. (Hashable a, Ord a, Ord c, Num c) 
      => (a -> HashMap a c)         -- ^ Neighbor function
      -> (a -> c)                   -- ^ Heuristic function
      -> (a -> Bool)                -- ^ Goal check
      -> a                          -- ^ Starting node
      -> Maybe [a]
astar neighbor heuristic goal root = 
    let h0 = heuristic root in go (Q.singleton root h0 h0) M.empty

    where go :: HashPSQ a c c -> HashMap a ([a], c) -> Maybe [a]
          go (Q.minView -> Nothing) _ = Nothing
          go (Q.minView -> Just (x, f, g, q')) past
              | goal x    = reverse . fst <$> M.lookup x past
              | otherwise = 
                  let xs  = neighbor x
                      q'' = M.foldlWithKey' (insertQ g) q' xs
                      p'  = _undefined
                   in go q'' p'
          
          insertQ g q y c = 
              let g' = g + c in Q.insert y (g' + heuristic y) g' q
