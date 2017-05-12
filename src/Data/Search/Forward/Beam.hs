{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Data.Search.Forward.Beam
(
    beam
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

data Beam a c = Beam 
    { beamLimit :: Word64
    , beamSize  :: Word64
    , beamCount :: Word64
    , beamWorst :: HashPSQ a (Down c) ()
    , beamFifo  :: HashPSQ a Word64 ()
    }

empty :: Natural -> Beam a c
empty n = Beam (fromIntegral n) 0 0 Q.empty Q.empty

insert :: (Hashable a, Ord a, Ord c) => a -> c -> Beam a c -> Beam a c
insert a c b
    | beamSize b < beamLimit b =
        let w' = Q.insert a (Down c) () $ beamWorst b
            f' = Q.insert a (beamCount b) () $ beamFifo b
         in b { beamSize  = succ (beamSize b)
              , beamCount = succ (beamCount b)
              , beamWorst = w'
              , beamFifo  = f' }
    | otherwise =
        case Q.findMin (beamWorst b) of
            Nothing  -> b
            Just (_, (Down worst), _) ->
                if c > worst then b else insert a c (kick b)

kick :: (Hashable a, Ord a, Ord c) => Beam a c -> Beam a c
kick b
    | Just (x, _, _, w') <- Q.minView (beamWorst b) =
        let f' = Q.delete x (beamFifo b)
         in b { beamSize  = pred (beamSize b)
              , beamWorst = w'
              , beamFifo  = f'
              }
    | otherwise = b

nextView :: (Hashable a, Ord a, Ord c) => Beam a c -> Maybe (a, Beam a c)
nextView b = do
    (x, _, _, f') <- Q.minView (beamFifo b)
    let w' = Q.delete x (beamWorst b)
        b' = b { beamSize  = pred (beamSize b)
               , beamWorst = w'
               , beamFifo  = f'
               }
    return (x, b')

-- | __Beam search__. There are a variety of beam search variations to be found
-- in the literature, some of which have a global beam, and some of which limit
-- expansion per level. The variation here always keeps the @k@ best successors
-- /globally/. it otherwise acts exactly like breadth first search.
--
-- It is not optimal in the general case.
beam :: forall a c t. (Foldable t, Hashable a, Ord a, Ord c)
     => Natural                     -- ^ Beam width
     -> (a -> t a)                  -- ^ Successor function
     -> (a -> c)                    -- ^ Heuristic function
     -> (a -> Bool)                 -- ^ Goal check
     -> a                           -- ^ Starting node
     -> Maybe [a]
beam width neighbor heuristic goal root = 
    let q0 = insert root (heuristic root) (empty width)
        (x, (m, _)) = runState search (HM.empty, q0)
     in x >>= fmap reverse . reconstruct m

    where search :: State (HashMap a a, Beam a c) (Maybe a)
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
