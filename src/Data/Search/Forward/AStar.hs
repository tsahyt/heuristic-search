{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Data.Search.Forward.AStar where

import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)

import qualified Data.HashMap.Lazy as M
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
          go (Q.minView -> Just (x, _, g, q)) past
              | goal x    = Just . reverse $ reconstruct past x
              | otherwise = 
                  let xs  = neighbor x
                      q'  = foldl' (\z (y,c) -> 
                                alter' (updateQ (g + heuristic y) (g + c)) y z) 
                                    q xs
                      p'  = foldl' (\m (y,c) -> 
                                M.alter (updateM x (g + c)) y m) 
                                    past xs
                   in go q' p'
          go _ _ = error "impossible"
          
          reconstruct past x
              | x == root = [x]
              | Just (x',_) <- M.lookup x past = x : reconstruct past x'
              | otherwise = []
          
updateQ :: Ord c => c -> c -> Maybe (c,c) -> (Maybe (c,c))
updateQ f g Nothing = Just (f, g)
updateQ f g (Just (f',g')) = if f < f' then Just (f,g) else Just (f',g')
{-# INLINE updateQ #-}

updateM :: Ord c => a -> c -> Maybe (a, c) -> Maybe (a, c)
updateM x c Nothing  = Just (x, c)
updateM x c (Just (x',c')) = if c < c' then Just (x, c) else Just (x', c')
{-# INLINE updateM #-}

-- | Like 'Data.HashPSQ.alter' but without the optional return parameter.
alter' :: (Ord p, Ord k, Hashable k) 
       => (Maybe (p, v) -> Maybe (p, v)) -> k -> HashPSQ k p v -> HashPSQ k p v
alter' f k = snd . Q.alter (\x -> ((), f x)) k
{-# INLINE alter' #-}
