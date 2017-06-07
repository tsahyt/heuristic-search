{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Data.Search.Forward.AStar 
(
    astar,
    astar',
    dijkstra,
    dijkstra',
    idastar,
    idastar'
)
where

import Control.Monad.ST
import Control.Monad

import Data.Bifunctor
import Data.Foldable
import Data.Semigroup
import Data.Hashable
import Data.HashTable.ST.Basic (HashTable)
import Data.HashPSQ (HashPSQ)

import qualified Data.HashTable.ST.Basic as T
import qualified Data.HashPSQ as Q

-- | __A* search algorithm__ for graphs generalized to functions. The neighbor
-- function describes all outgoing arcs from a given node of type @a@ as some
-- 'Foldable' collection. The tuples describe neighbors of type @a@, reachable
-- over edges of label type @b@ with cost of type @c@.
--
-- The heuristic function is given as a simple node evaluation function, @a ->
-- c@. The goal check is given in the same spirit as @a -> Bool@. Finally the
-- algorithm requires some node to start from.
--
-- Note that the heuristic /must/ be >= 0 for all nodes! Negative heuristic
-- values will be harmful!
--
-- Due to the construction of the algorithm, all nodes are restricted to a
-- finite number of neighbors.
astar :: forall a b c t. (Foldable t, Hashable a, Ord a, Ord c, Num c)
      => (a -> t (a, b, c))         -- ^ Neighbor function
      -> (a -> c)                   -- ^ Heuristic function
      -> (a -> Bool)                -- ^ Goal check
      -> a                          -- ^ Starting node
      -> Maybe [b]
astar neighbor heuristic goal root = 
    let h0 = heuristic root 
     in runST $ do
            ht <- T.new
            go (Q.singleton root h0 0) ht

    where go :: HashPSQ a c c -> HashTable s a (a, b, c) -> ST s (Maybe [b])
          go (Q.minView -> Nothing) _ = pure Nothing
          go (Q.minView -> Just (!x, _, !g, q)) past
              | goal x    = Just . reverse <$> reconstruct past x
              | otherwise = do
                  let xs = neighbor x
                  q' <- foldForM q xs $ \z (y, l, c) -> do
                      previousBest <- T.lookup past y
                      case previousBest of
                          Nothing -> 
                              T.insert past y (x, l, c)
                          Just (_, _, c') -> 
                              when (g + c < c') $ T.insert past y (x, l, g + c)
                      return $ alter' (updateQ (g + heuristic y) (g + c)) y z
                  go q' past
          go _ _ = error "impossible"
                  
          reconstruct past x
              | x == root = pure []
              | otherwise = T.lookup past x >>= \case
                  Nothing       -> pure []
                  Just (x',l,_) -> (l :) <$> reconstruct past x'
{-# INLINEABLE astar #-}

foldForM :: (Monad m, Foldable t) => b -> t a -> (b -> a -> m b) -> m b
foldForM b xs f = foldlM f b xs
{-# INLINE foldForM #-}

-- | Like 'astar' but without labelled edges.
astar' :: (Functor t, Foldable t, Num c, Ord c, Ord a, Hashable a)
       => (a -> t (a, c))           -- ^ Neighbor function
       -> (a -> c)                  -- ^ Heuristic function
       -> (a -> Bool)               -- ^ Goal check
       -> a                         -- ^ Starting node
       -> Maybe [a]
astar' neighbor heuristic goal root =
    let neighbor' = fmap (fmap (\(!a,!c) -> (a,a,c))) neighbor
     in (root :) <$> astar neighbor' heuristic goal root
{-# INLINEABLE astar' #-}

-- | Convenience function to use A* as Dijkstras algorithm by setting the
-- heuristic to be 0 for all nodes. Note that this is likely not the most
-- performant Dijkstra implementation because it retains all overhead from
-- handling the (non-existent) heuristic.
dijkstra :: (Foldable t, Num c, Ord c, Ord a, Hashable a) 
    => (a -> t (a, b, c)) -> (a -> Bool) -> a -> Maybe [b]
dijkstra neighbor goal root = astar neighbor (const 0) goal root
{-# INLINE dijkstra #-}

-- | Convenience function to use A* as Dijkstras algorithm by setting the
-- heuristic to be 0 for all nodes, enumerating nodes analogous to 'astar''.
-- Note that this is likely not the most performant Dijkstra implementation
-- because it retains all overhead from handling the (non-existent) heuristic.
dijkstra' :: (Functor t, Foldable t, Num c, Ord c, Ord a, Hashable a) 
    => (a -> t (a, c)) -> (a -> Bool) -> a -> Maybe [a]
dijkstra' neighbor goal root = astar' neighbor (const 0) goal root
{-# INLINE dijkstra' #-}
          
updateQ :: Ord c => c -> c -> Maybe (c,c) -> Maybe (c,c)
updateQ f g Nothing = Just (f, g)
updateQ f g (Just (f',g')) = if f < f' then Just (f,g) else Just (f',g')
{-# INLINE updateQ #-}

-- | Like 'Data.HashPSQ.alter' but without the optional return parameter.
alter' :: (Ord p, Ord k, Hashable k) 
       => (Maybe (p, v) -> Maybe (p, v)) -> k -> HashPSQ k p v -> HashPSQ k p v
alter' f k = snd . Q.alter (\x -> ((), f x)) k
{-# INLINE alter' #-}

newtype AltMin l r = AltMin { altMin :: Either (Option (Min l)) r }
    deriving (Show, Eq)

instance Ord l => Monoid (AltMin l r) where
    mempty = AltMin (Left (Option Nothing))
    mappend (AltMin (Right x)) _ = AltMin (Right x)
    mappend (AltMin (Left _)) (AltMin (Right y)) = AltMin (Right y)
    mappend (AltMin (Left x)) (AltMin (Left y)) = AltMin (Left $ x <> y)

-- | __Iterative Deepening A*__. This function does /not/ work with 0-cost
-- loops, i.e. nodes having themselves as 0 cost successors.
idastar :: forall a b c t. (Functor t, Foldable t, Ord c, Num c)
        => (a -> t (a, b, c))         -- ^ Neighbor function
        -> (a -> c)                   -- ^ Heuristic function
        -> (a -> Bool)                -- ^ Goal check
        -> a                          -- ^ Starting node
        -> Maybe [b]
idastar neighbor heuristic goal root = deepen (heuristic root)
    where deepen limit =
              case go root limit (heuristic root, 0) of
                  Left (Just f) -> deepen f
                  Left Nothing -> Nothing
                  Right x -> Just x

          go :: a -> c -> (c,c) -> Either (Maybe c) [b]
          go !x limit (!f, !g)
              | goal x    = Right []
              | f > limit = Left (Just f)
              | otherwise =
                  let xs = (\(x',l,c) -> (l :) 
                       <$> go x' limit (g + c + heuristic x', g + c)) 
                       <$> neighbor x
                   in first (fmap getMin . getOption) . altMin 
                    . foldMap (AltMin . first (Option . fmap Min)) $ xs
{-# INLINEABLE idastar #-}

-- | Like 'idastar', but without edge labels.
idastar' :: (Functor t, Foldable t, Ord c, Num c)
         => (a -> t (a, c)) 
         -> (a -> c) 
         -> (a -> Bool) 
         -> a 
         -> Maybe [a]
idastar' neighbor heuristic goal root =
    let neighbor' = fmap (fmap (\(a,c) -> (a,a,c))) neighbor
     in (root :) <$> idastar neighbor' heuristic goal root
{-# INLINEABLE idastar' #-}
