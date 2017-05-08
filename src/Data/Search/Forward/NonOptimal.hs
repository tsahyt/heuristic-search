module Data.Search.Forward.NonOptimal
(
    dft,
    dftT,
    dfs
)
where

import Data.Foldable
import Data.Hashable

import qualified Data.HashSet as HS

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

-- | 'dfs' provides a depth first search of a graph with unlabeled edges. Nodes
-- are returned in the order they are visted.
dfs :: (Foldable t, Eq a, Hashable a)
    => (a -> t a)           -- ^ Successor function
    -> (a -> Bool)          -- ^ Goal check
    -> t a                  -- ^ Starting list of nodes
    -> Maybe [a]
dfs suc goal = dfs' HS.empty . toList
    where dfs' visited (n:ns)
              | goal n = Just [n]
              | n `HS.member` visited = dfs' visited ns
              | otherwise = (n :) <$> dfs' (n `HS.insert` visited) 
                                           (toList (suc n) ++ ns)
          dfs' _ [] = Nothing
