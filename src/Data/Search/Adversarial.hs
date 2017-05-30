{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Search.Adversarial
(
    Player,
    negmax,
    minimax
)
where

import Data.Hashable
import Data.Typeable
import Data.Data
import GHC.Generics
import Data.Ord
import Data.Bifunctor
import Data.Foldable
import Numeric.Natural

data Player 
    = MinPlayer 
    | MaxPlayer
    deriving (Eq, Show, Ord, Read, Generic, Typeable, Data)

instance Hashable Player

-- | __Negmax__ (also called Negamax) algorithm for zero-sum games. Takes a
-- player dependent move function, a depth limit (ideally an even number), a
-- leaf evaluation function, and a leaf check. Operates from the view of the
-- maximizing player.
--
-- Returns the best move as determined by the search from the starting node.
negmax :: forall a b c t. (Foldable t, Functor t, Ord c, Num c)
       => Natural                   -- ^ Depth limit
       -> (Player -> a -> t (a, b)) -- ^ Moves
       -> (a -> c)                  -- ^ Evaluation function
       -> (a -> Bool)               -- ^ Terminal check
       -> a                         -- ^ Root node
       -> Maybe b
negmax cutoff suc eval term root =
    let xs = first (go (fromIntegral cutoff) False) <$> suc MaxPlayer root
     in if null xs 
        then Nothing 
        else Just . snd . maximumBy (comparing fst) $ xs
    where go :: Int -> Bool -> a -> c
          go 0 _ x = eval x
          go d p x 
              | term x = eval x
              | otherwise =
                    let xs = if p then suc MaxPlayer x else suc MinPlayer x
                     in if null xs then eval x 
                        else maximum 
                      . fmap (negate . go (pred d) (not p) . fst) $ xs

-- | __Minimax__ algorithm. Takes a depth limit (ideally even), a player
-- dependent move function, leaf evaluation function, a terminal check, and a
-- starting node. Operates from the view of the maximizing player.
--
-- Returns the best move as determined by the search from the starting node.
minimax :: forall a b c t. (Foldable t, Functor t, Ord c, Num c)
        => Natural                   -- ^ Depth limit
        -> (Player -> a -> t (a, b)) -- ^ Max player moves
        -> (a -> c)                  -- ^ Evaluation function
        -> (a -> Bool)               -- ^ Terminal check
        -> a                         -- ^ Root node
        -> Maybe b
minimax cutoff suc eval term root =
    let xs = first (gomin (fromIntegral cutoff)) <$> suc MaxPlayer root
     in if null xs 
        then Nothing 
        else Just . snd . maximumBy (comparing fst) $ xs
    where gomax, gomin :: Int -> a -> c
          gomax 0 x = eval x
          gomax d x
              | term x = eval x
              | otherwise =
                    let xs = suc MaxPlayer x
                     in if null xs then eval x
                        else maximum . fmap (gomin (pred d) . fst) $ xs

          gomin 0 x = eval x
          gomin d x
              | term x = eval x
              | otherwise =
                    let xs = suc MinPlayer x
                     in if null xs then eval x
                        else minimum . fmap (gomax (pred d) . fst) $ xs
