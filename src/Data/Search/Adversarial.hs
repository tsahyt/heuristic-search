{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Search.Adversarial
(
    Player (..),
    negmax,
    minimax
)
where

import Data.Bifunctor
import Data.Data
import Data.Foldable
import Data.Hashable
import Data.Inf
import Data.Ord
import GHC.Generics
import Numeric.Natural

data Player 
    = MinPlayer 
    | MaxPlayer
    deriving (Eq, Show, Ord, Read, Generic, Typeable, Data)

instance Hashable Player

turn :: Player -> Player
turn MinPlayer = MaxPlayer
turn MaxPlayer = MinPlayer

-- | __Negmax__ (also called Negamax) algorithm for zero-sum games. Takes a
-- player dependent move function, a depth limit (ideally an even number), and a
-- leaf evaluation function. Operates from the view of the maximizing player.
--
-- Returns the best move as determined by the search from the starting node.
--
-- Uses alpha beta pruning starting from the second level. This only works for
-- well-behaved 'foldr' implementations, like the one for lists!
negmax :: forall a b c t. (Foldable t, Ord c, Num c)
       => Natural                   -- ^ Depth limit
       -> (Player -> a -> t (a, b)) -- ^ Moves
       -> (a -> c)                  -- ^ Evaluation function
       -> a                         -- ^ Root node
       -> Maybe b
negmax cutoff suc eval root =
    let xs = first (go (fromIntegral cutoff) MinPlayer (NegInf, PosInf)) 
         <$> toList (suc MaxPlayer root)
     in if null xs 
        then Nothing 
        else Just . snd . maximumBy (comparing fst) $ xs
    where go :: Int -> Player -> (Inf c, Inf c) -> a -> Inf c
          go 0 _ _ x = Fin $ eval x
          go d p (alpha, beta) x =
              let xs = suc p x
                  f (a,_) next z
                      | z >= beta = z
                      | otherwise = next . max z . negate 
                                  . go (pred d) (turn p) (-beta, -z)
                                  $ a
               in if null xs then Fin (eval x) else foldr f id xs alpha

-- | __Minimax__ algorithm. Takes a depth limit (ideally even), a player
-- dependent move function, leaf evaluation function, and a starting node.
-- Operates from the view of the maximizing player.
--
-- Returns the best move as determined by the search from the starting node.
--
-- Uses alpha beta pruning starting from the second level. This only works for
-- well-behaved 'foldr' implementations, like the one for lists!
minimax :: forall a b c t. (Foldable t, Ord c, Num c)
        => Natural                   -- ^ Depth limit
        -> (Player -> a -> t (a, b)) -- ^ Max player moves
        -> (a -> c)                  -- ^ Evaluation function
        -> a                         -- ^ Root node
        -> Maybe b
minimax cutoff suc eval root =
    let xs = first (gomin (fromIntegral cutoff) (NegInf, PosInf)) 
         <$> toList (suc MaxPlayer root)
     in if null xs 
        then Nothing 
        else Just . snd . maximumBy (comparing fst) $ xs
    where gomax, gomin :: Int -> (Inf c, Inf c) -> a -> Inf c
          gomax 0 _ x = Fin $ eval x
          gomax d (alpha, beta) x =
              let xs = suc MaxPlayer x
                  f (a,_) next z
                      | z >= beta = z
                      | otherwise = 
                          next . max z . gomin (pred d) (z, beta) $ a
               in if null xs then Fin (eval x) else foldr f id xs alpha

          gomin 0 _ x = Fin $ eval x
          gomin d (alpha, beta) x =
              let xs = suc MaxPlayer x
                  f (a,_) next z
                      | z <= alpha = z
                      | otherwise  = 
                          next . min z . gomax (pred d) (alpha, z) $ a
               in if null xs then Fin (eval x) else foldr f id xs beta
