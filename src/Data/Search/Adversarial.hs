{-# LANGUAGE ScopedTypeVariables #-}
module Data.Search.Adversarial
(
    negmax,
    minimax
)
where

import Data.Ord
import Data.Bifunctor
import Data.Foldable
import Numeric.Natural

negmax :: forall a b c t. (Foldable t, Functor t, Ord c, Num c)
       => Natural                   -- ^ Depth limit
       -> (a -> t (a, b))           -- ^ Max player moves
       -> (a -> t (a, b))           -- ^ Min player moves
       -> (a -> c)                  -- ^ Evaluation function
       -> (a -> Bool)               -- ^ Terminal check
       -> a                         -- ^ Root node
       -> Maybe b
negmax cutoff maxSucc minSucc eval term root =
    let xs = fmap (first (go (fromIntegral cutoff) False)) (maxSucc root)
     in if null xs 
        then Nothing 
        else Just . snd . maximumBy (comparing fst) $ xs
    where go :: Int -> Bool -> a -> c
          go 0 _ x = eval x
          go d p x 
              | term x = eval x
              | otherwise =
                    let xs = if p then maxSucc x else minSucc x
                     in if null xs then eval x 
                        else maximum 
                      . fmap (negate . go (pred d) (not p) . fst) $ xs

minimax :: forall a b c t. (Foldable t, Functor t, Ord c, Num c)
        => Natural                   -- ^ Depth limit
        -> (a -> t (a, b))           -- ^ Max player moves
        -> (a -> t (a, b))           -- ^ Min player moves
        -> (a -> c)                  -- ^ Evaluation function
        -> (a -> Bool)               -- ^ Terminal check
        -> a                         -- ^ Root node
        -> Maybe b
minimax cutoff maxSucc minSucc eval term root =
    let xs = fmap (first (gomin (fromIntegral cutoff))) (maxSucc root)
     in if null xs 
        then Nothing 
        else Just . snd . maximumBy (comparing fst) $ xs
    where gomax, gomin :: Int -> a -> c
          gomax 0 x = eval x
          gomax d x
              | term x = eval x
              | otherwise =
                    let xs = maxSucc x 
                     in if null xs then eval x
                        else maximum . fmap (gomin (pred d) . fst) $ xs

          gomin 0 x = eval x
          gomin d x
              | term x = eval x
              | otherwise =
                    let xs = minSucc x
                     in if null xs then eval x
                        else minimum . fmap (gomax (pred d) . fst) $ xs
