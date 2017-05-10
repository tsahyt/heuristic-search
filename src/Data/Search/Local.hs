{-# LANGUAGE ScopedTypeVariables #-}
module Data.Search.Local
(
    hillClimb,
    rrHillClimb,
    enforcedHillClimb
)
where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Random.Class
import Data.Foldable
import Data.Hashable
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Ord
import Numeric.Natural
import System.Random (Random)

import Data.Search.Forward.NonOptimal (bfs)

import qualified Data.List.NonEmpty as N


-- | Perform __hill climbing optimization__ on a (discrete) state space given by
-- a neighbor function. The evaluation function determines the value of each
-- state, often also called objective function. Hill climbing only achieves
-- optimal solutions in convex problems.
--
-- The algorithm assumes that more is better, i.e. it searches for local
-- /maxima/. Use 'Down' to search for minima.
--
-- Note that every state must have at least one successor state! However, having
-- multiple similar successors is permitted. If multiple successors with the
-- same value are encountered, no guarantee is made as to which will be picked.
hillClimb :: forall a c. (Ord a, Ord c)
          => (a -> NonEmpty a)      -- ^ Neighbor function
          -> (a -> c)               -- ^ Evaluation function
          -> a                      -- ^ Starting node
          -> a
hillClimb neighbor eval start = last $ atHillClimb neighbor eval start
{-# INLINE hillClimb #-}

-- | Anytime variant of 'hillClimb'. The result is the full sequence of visited
-- states, and is generated lazily. It can therefore be consumed as necessary.
-- The last element is the local optimum found by the hill climbing search.
--
-- Result is guaranteed to be non-empty as at least the starting node is
-- contained.
atHillClimb :: forall a c. (Ord a, Ord c)
            => (a -> NonEmpty a)      -- ^ Neighbor function
            -> (a -> c)               -- ^ Evaluation function
            -> a                      -- ^ Starting node
            -> [a]
atHillClimb neighbor eval start = go start (eval start)
    where go x c = 
              let (next, c') = maximumBy (comparing snd) 
                             . fmap (\z -> (z, eval z)) . neighbor $ x
               in if c' <= c then x : go next c' else [x]
{-# INLINE atHillClimb #-}

-- | __Random restart hill climbing__. Utilizes 'hillClimb' underneath to
-- perform @k@ runs of the standard hill climbing algorithm, starting from
-- randomly generated states. The best (maximum) value across all runs will be
-- returned.
--
-- When the iteration count is 0, a random state will be returned. This is done
-- in order to keep the function total.
rrHillClimb :: forall m a c. (MonadRandom m, Random a, Ord a, Ord c)
            => Natural
            -> (a -> NonEmpty a)    -- ^ Neighbor function
            -> (a -> c)             -- ^ Evaluation function 
            -> m a
rrHillClimb 0 _ _ = getRandom
rrHillClimb n neighbor eval = 
    maximumBy (comparing eval) <$> replicateM (fromIntegral n) go
    where go = hillClimb neighbor eval <$> getRandom
{-# INLINABLE rrHillClimb #-}

-- | __Enforced hill climbing__ is a hill-climbing variant that picks a
-- successor note only if it has a strictly better heuristic evaluation than the
-- current node. This is a minimizing algorithm, to facilitate use with standard
-- A* like heuristics. As such the goal is assumed to have heuristic value 0.
--
-- Because not every node necessarily has a successor that is better than
-- itself, EHC performes breadth first search to find the next successor in this
-- case.
--
-- No backtracking is ever performed, and thus EHC is incomplete in directed
-- graphs.
enforcedHillClimb :: forall a c t. (Foldable t, Hashable a, Ord a, Ord c)
                  => (a -> t a)     -- ^ Neighbor function
                  -> (a -> c)       -- ^ Heuristic/Evaluation function
                  -> (a -> Bool)    -- ^ Goal check
                  -> a              -- ^ Starting node
                  -> Maybe [a]
enforcedHillClimb neighbor heuristic goal root = ehc root (heuristic root)
    where ehc :: a -> c -> Maybe [a]
          ehc u h
              | goal u    = Just [u] 
              | otherwise = do
                    (us, h') <- ehcBfs u h
                    let u' = N.last us
                    (N.init us ++) <$> ehc u' h'

          ehcBfs :: a -> c -> Maybe (NonEmpty a, c)
          ehcBfs u h = (id &&& heuristic . N.last) 
                   <$> (nonEmpty =<< bfs neighbor (\x -> heuristic x < h) u)
{-# INLINABLE enforcedHillClimb #-}
