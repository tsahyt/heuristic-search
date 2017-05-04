{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Data.Search.Forward.AStar where

import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashPSQ (HashPSQ)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.HashPSQ as Q

astar :: forall a c t. (Foldable t, Eq a, Hashable a, Ord a, Ord c, Num c)
      => (a -> t (a,c))             -- ^ Neighbor function
      -> (a -> c)                   -- ^ Heuristic function
      -> (a -> Bool)                -- ^ Goal check
      -> a                          -- ^ Starting node
      -> Maybe [a]
astar neighbor heuristic goal root = 
    let h0 = heuristic root 
     in go (Q.singleton root h0 0) M.empty

    where go :: HashPSQ a c c -> HashMap a (a, c) -> Maybe [a]
          go (Q.minView -> Nothing) _ = Nothing
          go (Q.minView -> Just (x, f, g, q)) past
              | goal x    = Just . reverse $ reconstruct past x
              | otherwise = 
                  let xs  = neighbor x
                      q'  = foldl' (\q (y,c) -> 
                                snd $ Q.alter (updateQ y (g + c)) y q) 
                                    q xs
                      p'  = foldl' (\m (y,c) -> 
                                M.alter (updateM x (g + c)) y m) 
                                    past xs
                   in go q' p'
          
          updateQ :: a -> c -> Maybe (c,c) -> ((), Maybe (c,c))
          updateQ y g Nothing = ((), Just (g + heuristic y, g))
          updateQ y g (Just (f',g')) =
              let f = g + heuristic y
               in if f < f' then ((), Just (f,g)) else ((), Just (f',g'))

          updateM x c Nothing  = Just (x, c)
          updateM x c (Just (x',c')) = 
              if c < c' then Just (x, c) else Just (x', c')
          
          reconstruct past x
              | x == root = [x]
              | Just (x',_) <- M.lookup x past = x : reconstruct past x'
              | otherwise = []
