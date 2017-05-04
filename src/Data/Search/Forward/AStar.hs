{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Data.Search.Forward.AStar where

import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashPSQ as Q

astar :: forall a b c t. (Foldable t, Eq a, Hashable a, Ord a, Ord c, Num c)
      => (a -> t (a, b, c))         -- ^ Neighbor function
      -> (a -> c)                   -- ^ Heuristic function
      -> (a -> Bool)                -- ^ Goal check
      -> a                          -- ^ Starting node
      -> Maybe [b]
astar neighbor heuristic goal root = 
    let h0 = heuristic root 
     in go (Q.singleton root h0 0) M.empty

    where go :: HashPSQ a c c -> HashMap a (a, b, c) -> Maybe [b]
          go (Q.minView -> Nothing) _ = Nothing
          go (Q.minView -> Just (x, _, g, q)) past
              | goal x    = Just . reverse $ reconstruct past x
              | otherwise = 
                  let xs = neighbor x
                      (q', p') = foldl' 
                          (\z (y, l, c) -> bimap 
                              (alter' (updateQ (g + heuristic y) (g + c)) y) 
                              (M.alter (updateM x l (g + c)) y) z) 
                          (q, past) xs
                   in go q' p'
          go _ _ = error "impossible"
          
          reconstruct past x
              | x == root = []
              | Just (x',l,_) <- M.lookup x past = l : reconstruct past x'
              | otherwise = []

-- | Like 'astar' but without labelled edges.
astar' :: (Functor t, Num c, Ord c, Ord a, Hashable a, Foldable t) 
       => (a -> t (a, c))           -- ^ Neighbor function
       -> (a -> c)                  -- ^ Heuristic function
       -> (a -> Bool)               -- ^ Goal check
       -> a                         -- ^ Starting node
       -> Maybe [a]
astar' neighbor heuristic goal root =
    let neighbor' = fmap (fmap (\(a,c) -> (a,a,c))) neighbor
     in fmap (root :) $ astar neighbor' heuristic goal root
          
updateQ :: Ord c => c -> c -> Maybe (c,c) -> (Maybe (c,c))
updateQ f g Nothing = Just (f, g)
updateQ f g (Just (f',g')) = if f < f' then Just (f,g) else Just (f',g')
{-# INLINE updateQ #-}

updateM :: Ord c => a -> b -> c -> Maybe (a, b, c) -> Maybe (a, b, c)
updateM x l c Nothing  = Just (x, l, c)
updateM x l c (Just (x', l', c')) = 
    if c < c' then Just (x, l, c) else Just (x', l', c')
{-# INLINE updateM #-}

-- | Like 'Data.HashPSQ.alter' but without the optional return parameter.
alter' :: (Ord p, Ord k, Hashable k) 
       => (Maybe (p, v) -> Maybe (p, v)) -> k -> HashPSQ k p v -> HashPSQ k p v
alter' f k = snd . Q.alter (\x -> ((), f x)) k
{-# INLINE alter' #-}
