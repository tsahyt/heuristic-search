module Data.Search.Forward.NonOptimal
(
    dft,
    dftT,
    dfs,
    dfsT
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
    -> a                    -- ^ Starting list of nodes
    -> Maybe [a]
dfs suc goal = go HS.empty
    where go visited n 
              | goal n = Just [n]
              | n `HS.member` visited = Nothing
              | otherwise = 
                    let v' = n `HS.insert` visited
                     in (n :) <$> asum [ go v' x | x <- toList (suc n) ]

dfsT :: (Functor t, Foldable t)
     => (a -> t a)          -- ^ Successor function
     -> (a -> Bool)         -- ^ Goal check
     -> a                   -- ^ Starting node
     -> Maybe [a]
dfsT suc goal = go
    where go n 
              | goal n    = Just [n]
              | otherwise = (n :) <$> asum [ go x | x <- toList (suc n) ]
