{-# LANGUAGE ScopedTypeVariables #-}
module Data.Search.Local
(
    hillClimb,
    rrHillClimb
)
where

import Control.Monad
import Control.Monad.Random.Class
import Data.Foldable
import Data.Ord
import Data.List.NonEmpty (NonEmpty)
import System.Random (Random)

import Numeric.Natural

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
hillClimb neighbor eval start = go start (eval start)
    where go x c = 
              let (next, c') = maximumBy (comparing snd) 
                             . fmap (\z -> (z, eval z)) . neighbor $ x
               in if c' <= c then go next c' else x
{-# INLINE hillClimb #-}

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
