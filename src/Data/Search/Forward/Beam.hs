{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Data.Search.Forward.Beam
(
    beamLocal
)
where

import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashPSQ (HashPSQ)
import Data.Hashable
import Data.Ord
import Data.Word
import Data.Bifunctor
import Data.Foldable
import Numeric.Natural

import qualified Data.HashPSQ as Q
import qualified Data.HashMap.Strict as HM

data LBeam a c = LBeam 
    { lbeamLimit :: Word64
    , lbeamSize  :: Word64
    , lbeamCount :: Word64
    , lbeamWorst :: HashPSQ a (Down c) ()
    , lbeamFifo  :: HashPSQ a Word64 ()
    }

empty :: Natural -> LBeam a c
empty n = LBeam (fromIntegral n) 0 0 Q.empty Q.empty

insert :: (Hashable a, Ord a, Ord c) => a -> c -> LBeam a c -> LBeam a c
insert a c b
    | lbeamSize b < lbeamLimit b =
        let w' = Q.insert a (Down c) () $ lbeamWorst b
            f' = Q.insert a (lbeamCount b) () $ lbeamFifo b
         in b { lbeamSize  = succ (lbeamSize b)
              , lbeamCount = succ (lbeamCount b)
              , lbeamWorst = w'
              , lbeamFifo  = f' }
    | otherwise =
        case Q.findMin (lbeamWorst b) of
            Nothing  -> b
            Just (_, (Down worst), _) ->
                if c > worst then b else insert a c (kick b)

kick :: (Hashable a, Ord a, Ord c) => LBeam a c -> LBeam a c
kick b
    | Just (x, _, _, w') <- Q.minView (lbeamWorst b) =
        let f' = Q.delete x (lbeamFifo b)
         in b { lbeamSize  = pred (lbeamSize b)
              , lbeamWorst = w'
              , lbeamFifo  = f'
              }
    | otherwise = b

nextView :: (Hashable a, Ord a, Ord c) => LBeam a c -> Maybe (a, LBeam a c)
nextView b = do
    (x, _, _, f') <- Q.minView (lbeamFifo b)
    let w' = Q.delete x (lbeamWorst b)
        b' = b { lbeamSize  = pred (lbeamSize b)
               , lbeamWorst = w'
               , lbeamFifo  = f'
               }
    return (x, b')

-- | __Beam local search__. Instead of keeping just one state in memory, this
-- algorithm always keeps the @k@ best steps in memory, always proceeding to the
-- currently best step. Note that this is not equivalent to @k@ hill climbing
-- steps, as the algorithm can and will jump between branches. It is essentially
-- a breadth first search with a limited and self ordering open list.
--
-- For @k = 1@, this is equivalent to hill climbing. For @k = inf@, it becomes
-- (heuristic) breadth first search. When you need @k = 1@, the 'hillClimb'
-- family of functions will perform much better.
beamLocal :: forall a c t. (Foldable t, Hashable a, Ord a, Ord c)
          => Natural                     -- ^ Beam width
          -> (a -> t a)                  -- ^ Successor function
          -> (a -> c)                    -- ^ Heuristic function
          -> (a -> Bool)                 -- ^ Goal check
          -> a                           -- ^ Starting node
          -> Maybe [a]
beamLocal width neighbor heuristic goal root = 
    let q0 = insert root (heuristic root) (empty width)
        (x, (m, _)) = runState search (HM.empty, q0)
     in x >>= fmap reverse . reconstruct m

    where search :: State (HashMap a a, LBeam a c) (Maybe a)
          search = nextView <$> gets snd >>= \case
              Nothing -> pure Nothing
              Just (x, b) -> if goal x then pure (Just x) else do
                  m <- gets fst
                  let xs = filter (not . (`HM.member` m)) 
                         . toList . neighbor $ x
                  modify $ bimap 
                      (\z -> foldl' (flip (`HM.insert` x)) z xs)
                      (\z -> foldl' (flip (ap insert heuristic)) z xs)
                      . second (const b)
                  search
        
          reconstruct :: HashMap a a -> a -> Maybe [a]
          reconstruct m x
              | x == root = Just [x]
              | Just x' <- x `HM.lookup` m = (x :) <$> reconstruct m x'
              | otherwise = Nothing
