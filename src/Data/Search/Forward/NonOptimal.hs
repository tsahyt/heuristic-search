-- | Various simple traversals and primitive search functions for generalized
-- graphs.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Data.Search.Forward.NonOptimal
(
    -- * Depth First
    dft,
    dftG,
    dfs,
    dfs',
    dfsG,

    -- * Depth Limited
    dlt,
    dls,
    dls',
    ids,
    ids',

    -- * Breadth First
    bft,
    bftT,
    bfs,
    bfs'
)
where

import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Deque (Deque)

import Numeric.Natural

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Deque as DQ

-- | Like 'dft' but using a visited set. This function can cope with cycles in
-- the graph. In return, memory complexity is no longer linear in the search
-- depth!
dftG :: (Foldable t, Eq a, Hashable a)
     => (a -> t a)           -- ^ Successor function
     -> t a                  -- ^ Starting nodes
     -> [a]
dftG suc = go HS.empty . toList
    where go visited (n:ns)
              | n `HS.member` visited = go visited ns
              | otherwise = n : go (n `HS.insert` visited) 
                                   (toList (suc n) ++ ns)
          go _ [] = []
{-# INLINABLE dftG #-}

-- | 'dft' provides a simple depth first traversal of a graph with unlabeled
-- edges. Nodes are returned in the order they are visted. In the presence of
-- cycles, this function may loop indefinitely.
dft :: Foldable t
    => (a -> t a)           -- ^ Successor function
    -> t a                  -- ^ Starting nodes
    -> [a]
dft suc = go . toList
    where go (n:ns) = n : go (toList (suc n) ++ ns)
          go [] = []
{-# INLINE dft #-}

-- | Like 'dfs' but using a visited set, i.e. doing graph search instead of tree
-- search. This function can cope with cycles in the graph. In return, memory
-- complexity is no longer linear in the search depth!
dfsG :: (Foldable t, Eq a, Hashable a)
     => (a -> t a)           -- ^ Successor function
     -> (a -> Bool)          -- ^ Goal check
     -> a                    -- ^ Starting node
     -> Maybe [a]
dfsG suc goal = go HS.empty
    where go visited n 
              | goal n = Just [n]
              | n `HS.member` visited = Nothing
              | otherwise = 
                    let v' = n `HS.insert` visited
                     in (n :) <$> asum [ go v' x | x <- toList (suc n) ]
{-# INLINABLE dfsG #-}

-- | 'dfs' provides a depth first search of a graph with labeled edges.
-- Returns exactly the discovered path from root to the first goal, if any. In
-- the presence of cycles, this function may loop indefinitely.
dfs :: (Functor t, Foldable t)
    => (a -> t (a, b))     -- ^ Successor function
    -> (a -> Bool)         -- ^ Goal check
    -> a                   -- ^ Starting node
    -> Maybe [b]
dfs suc goal root = mapMaybe snd <$> 
    dfs' (fmap (fmap pure) . suc . fst) (goal . fst) (root, Nothing)
{-# INLINE dfs #-}

-- | Like 'dfs', but for unlabelled edges.
dfs' :: Foldable t
     => (a -> t a)          -- ^ Successor function
     -> (a -> Bool)         -- ^ Goal check
     -> a                   -- ^ Starting node
     -> Maybe [a]
dfs' suc goal = go
    where go n 
              | goal n    = Just [n]
              | otherwise = (n :) <$> asum [ go x | x <- toList (suc n) ]
{-# INLINE dfs' #-}

-- | 'dlt' performs a depth limited traversal. Nodes are returned in the order
-- they are enountered.
--
-- This function can be used to traverse an infinite tree (or graph) to a given
-- depth. No visited set is being maintained, space complexity is thus linear.
dlt :: Foldable t
    => Natural              -- ^ Depth limit
    -> (a -> t a)           -- ^ Successor function
    -> a                    -- ^ Starting node
    -> [a]
dlt limit suc = go limit
    where go 0 x = [x]
          go l x = toList (suc x) >>= go (pred l)
{-# INLINE dlt #-}

-- | 'dls' provides a depth limited search of a graph with labeled edges.
-- Returns exactly the discovered path from root to the first goal, if any. Due
-- to the depth limit, this function can cope with cycles.
dls :: (Functor t, Foldable t) 
    => Natural              -- ^ Depth limit
    -> (a -> t (a, b))      -- ^ Successor function
    -> (a -> Bool)          -- ^ Goal check
    -> a                    -- ^ Starting node
    -> Maybe [b]
dls limit suc goal root = mapMaybe snd <$> 
    dls' limit (fmap (fmap pure) . suc . fst) (goal . fst) (root, Nothing)
{-# INLINE dls #-}

-- | 'dls'' provides a depth limited search of a graph with unlabeled edges.
-- Returns exactly the discovered path from root to the first goal, if any. Due
-- to the depth limit, this function can cope with cycles.
dls' :: Foldable t
     => Natural              -- ^ Depth limit
     -> (a -> t a)           -- ^ Successor function
     -> (a -> Bool)          -- ^ Goal check
     -> a                    -- ^ Starting node
     -> Maybe [a]
dls' limit suc goal = go limit
    where go 0 _ = Nothing
          go l n 
              | goal n    = Just [n]
              | otherwise = (n :) <$> asum 
                                [ go (pred l) x | x <- toList (suc n) ]
{-# INLINE dls' #-}

-- | 'ids' performs iterative deepening search of a graph with labeled edges.
-- Returns exactly the discovered path from root to the first goal. If there is
-- no path, it will loop indefinitely!
--
-- An upper bound on the search depth can be set optionally with the first
-- parameter.
ids :: (Functor t, Foldable t)
    => Maybe Natural
    -> (a -> t (a, b)) 
    -> (a -> Bool) 
    -> a 
    -> Maybe [b]
ids limit suc goal root = 
    listToMaybe $ mapMaybe (\l -> dls l suc goal root) 
                           (maybe [1..] (\x -> [1..x]) limit)
{-# INLINEABLE ids #-}

-- | 'ids' performs iterative deepening search of a graph with unlabeled edges.
-- Returns exactly the discovered path from root to the first goal. If there is
-- no path, it will loop indefinitely!
--
-- An upper bound on the search depth can be set optionally with the first
-- parameter.
ids' :: Foldable t 
     => Maybe Natural
     -> (a -> t a) 
     -> (a -> Bool) 
     -> a 
     -> Maybe [a]
ids' limit suc goal root = 
    listToMaybe $ mapMaybe (\l -> dls' l suc goal root) 
                           (maybe [1..] (\x -> [1..x]) limit)
{-# INLINEABLE ids' #-}

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
{-# INLINABLE bft #-}

-- | Like 'bft' but traversing a tree. This has the potential to loop
-- indefinitely in graphs with cycles!
bftT :: Foldable t
     => (a -> t a)
     -> t a
     -> [a]
bftT suc = go . toList
    where go [] = []
          go (n:ns) = n : go (ns ++ toList (suc n))
{-# INLINE bftT #-}

-- | 'bfs' performes a breadth first search of a graph with unlabeled edges.
-- Returns exactly the discovered path from root to the first goal, if any.
--
-- Breadth first search is optimal if the path cost is a nondecreasing function
-- of the depth of the node. 'bfs' is placed in the "NonOptimal" module since
-- costs cannot be arbitrary.
bfs' :: forall a t. (Foldable t, Hashable a, Eq a)
     => (a -> t a)
     -> (a -> Bool)
     -> a
     -> Maybe [a]
bfs' suc goal root = 
    let (x, (m, _)) = runState search (HM.empty, DQ.fromList . pure $ root)
     in x >>= fmap reverse . reconstruct m

    where search :: State (HashMap a a, Deque a) (Maybe a)
          search = DQ.uncons <$> gets snd >>= \case
              Nothing -> pure Nothing
              Just (x, q) -> if goal x then pure (Just x) else do
                  m <- gets fst
                  let xs = filter (not . (`HM.member` m)) 
                         . toList . suc $ x
                  modify $ bimap 
                      (\z -> foldl' (flip (`HM.insert` x)) z xs)
                      (DQ.prepend . DQ.fromList . toList $ xs)
                      . second (const q)
                  search
        
          reconstruct :: HashMap a a -> a -> Maybe [a]
          reconstruct m x
              | x == root = Just [x]
              | Just x' <- x `HM.lookup` m = (x :) <$> reconstruct m x'
              | otherwise = Nothing

bfs :: forall a b t. (Foldable t, Hashable a, Eq a)
     => (a -> t (a, b))
     -> (a -> Bool)
     -> a
     -> Maybe [b]
bfs suc goal root = 
    let (x, (m, _)) = runState search (HM.empty, DQ.fromList . pure $ root)
     in x >>= fmap reverse . reconstruct m

    where search :: State (HashMap a (a, b), Deque a) (Maybe a)
          search = DQ.uncons <$> gets snd >>= \case
              Nothing -> pure Nothing
              Just (x, q) -> if goal x then pure (Just x) else do
                  m <- gets fst
                  let xs = filter (not . (`HM.member` m) . fst) 
                         . toList . suc $ x
                  modify $ bimap 
                      (\z -> foldl' (\hm from -> HM.insert x from hm) z xs)
                      (DQ.prepend . DQ.fromList . map fst . toList $ xs)
                      . second (const q)
                  search
        
          reconstruct :: HashMap a (a, b) -> a -> Maybe [b]
          reconstruct m x
              | x == root = Just []
              | Just (x', l) <- x `HM.lookup` m = (l :) <$> reconstruct m x'
              | otherwise = Nothing
