{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Data.Search.Forward.NonOptimal
(
    -- * Depth First
    dft,
    dftT,
    dfs,
    dfsT,

    -- * Breadth First
    bft,
    bftT,
    bfs
)
where

import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Deque (Deque)

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Deque as DQ

-- | 'dft' provides a simple depth first traversal of a graph with unlabeled
-- edges. Nodes are returned in the order they are visted.
dft :: (Foldable t, Eq a, Hashable a)
    => (a -> t a)           -- ^ Successor function
    -> t a                  -- ^ Starting nodes
    -> [a]
dft suc = go HS.empty . toList
    where go visited (n:ns)
              | n `HS.member` visited = go visited ns
              | otherwise = n : go (n `HS.insert` visited) 
                                   (toList (suc n) ++ ns)
          go _ [] = []

-- | Like 'dft' but traversing a tree. This has the potential to loop
-- indefinitely in graphs with cycles!
dftT :: Foldable t
     => (a -> t a)           -- ^ Successor function
     -> t a                  -- ^ Starting nodes
     -> [a]
dftT suc = go . toList
    where go (n:ns) = n : go (toList (suc n) ++ ns)
          go [] = []

-- | 'dfs' provides a depth first search of a graph with unlabeled edges.
-- Returns exactly the discovered path from root to the first goal, if any.
dfs :: (Foldable t, Eq a, Hashable a)
    => (a -> t a)           -- ^ Successor function
    -> (a -> Bool)          -- ^ Goal check
    -> a                    -- ^ Starting list of nodes
    -> Maybe [a]
dfs suc goal = go HS.empty
    where go visited n 
              | goal n = Just [n]
              | n `HS.member` visited = Nothing
              | otherwise = 
                    let v' = n `HS.insert` visited
                     in (n :) <$> asum [ go v' x | x <- toList (suc n) ]

-- | Like 'dfs' but traversing a tree. This has the potential to loop
-- indefinitely in graphs with cycles!
dfsT :: Foldable t
     => (a -> t a)          -- ^ Successor function
     -> (a -> Bool)         -- ^ Goal check
     -> a                   -- ^ Starting node
     -> Maybe [a]
dfsT suc goal = go
    where go n 
              | goal n    = Just [n]
              | otherwise = (n :) <$> asum [ go x | x <- toList (suc n) ]

-- | 'bft' performs a breadth first traversal of a graph with unlabeled edges.
-- Nodes are returned in the order they are visited.
bft :: (Foldable t, Hashable a, Eq a)
    => (a -> t a)
    -> t a
    -> [a]
bft suc = go HS.empty . toList
    where go _ [] = []
          go visited (n:ns) 
              | n `HS.member` visited = go visited ns
              | otherwise = n : go (n `HS.insert` visited) 
                                   (ns ++ toList (suc n))

-- | Like 'bft' but traversing a tree. This has the potential to loop
-- indefinitely in graphs with cycles!
bftT :: Foldable t
     => (a -> t a)
     -> t a
     -> [a]
bftT suc = go . toList
    where go [] = []
          go (n:ns) = n : go (ns ++ toList (suc n))

-- | 'bfs' performes a breadth first search of a graph with unlabeled edges.
-- Returns exactly the discovered path from root to the first goal, if any.
--
-- Breadth first search is optimal if the path cost is a nondecreasing function
-- of the depth of the node. 'bfs' is placed in the "NonOptimal" module since
-- costs cannot be arbitrary.
bfs :: forall a t. (Foldable t, Hashable a, Eq a)
    => (a -> t a)
    -> (a -> Bool)
    -> a
    -> Maybe [a]
bfs suc goal root = 
    let (x, (m, _)) = runState search (HM.empty, DQ.fromList . pure $ root)
     in x >>= fmap reverse . reconstruct m

    where search :: State (HashMap a a, Deque a) (Maybe a)
          search = DQ.uncons <$> gets snd >>= \case
              Nothing -> pure Nothing
              Just (x, q) -> if goal x then pure (Just x) else do
                  let xs = suc x
                  modify $ bimap 
                      (\m -> foldl' (flip (`HM.insert` x)) m xs)
                      (DQ.prepend . DQ.fromList . toList $ xs)
                      . second (const q)
                  search
        
          reconstruct :: HashMap a a -> a -> Maybe [a]
          reconstruct m x
              | x == root = Just [x]
              | Just x' <- x `HM.lookup` m = (x :) <$> reconstruct m x'
              | otherwise = Nothing
