{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Search.Forward.RBFS
(
    rbfs,
    rbfs'
)
where

import Data.Inf
import Data.Foldable
import Data.Hashable
import qualified Data.HashPSQ as Q

-- | __Recursive best first search__. The neighbor function describes all
-- outgoing arcs from a given node of type @a@ as some 'Foldable' collection.
-- The tuples describe neighbors of type @a@, reachable over edges of label type
-- @b@ with cost type @c@.
--
-- The heuristic function is given as a node evaluation @a -> c@. The goal check
-- is given as an @a -> Bool@.
--
-- The heuristic must be @>= 0@. Admissibility is required for optimal paths.
-- Results from the neighbor function /must/ be finite, as the algorithm
-- internally utilizes a queue, the building of which cannot terminate for
-- infinite inputs.
rbfs :: forall a b c t. (Foldable t, Hashable a, Ord a, Ord c, Num c)
     => (a -> t (a, b, c))         -- ^ Neighbor function
     -> (a -> c)                   -- ^ Heuristic function
     -> (a -> Bool)                -- ^ Goal check
     -> a                          -- ^ Starting node
     -> Maybe [b]
rbfs neighbor heuristic goal root = 
    case go root PosInf (Fin $ heuristic root, 0) of
        Left  _ -> Nothing
        Right x -> Just x

    where go :: a -> Inf c -> (Inf c, c) -> Either (Inf c) [b]
          go x fLimit (f, g)
              | goal x = Right []
              | otherwise =
                  let xs = Q.fromList . map (buildS f g)
                         . toList . neighbor $ x
                   in loop fLimit xs
          
          buildS f g (a, b, c) =
              let g' = g + c
                  f' = max (Fin $ g' + heuristic a) f
               in (a, f', (b, g'))

          loop _ (Q.minView -> Nothing) = Left PosInf
          loop fLimit (Q.minView -> Just (x, f, (b, g), q))
              | f > fLimit = Left f
              | otherwise =
                  let alt = maybe PosInf mid . Q.findMin $ q
                   in case go x (min fLimit alt) (f, g) of
                          Left f' -> loop fLimit (Q.insert x f' (b, g) q)
                          Right z -> Right $ b : z

          loop _ _ = error "impossible"
{-# INLINABLE rbfs #-}

-- | Like 'rbfs' but without edge labels.
rbfs' :: forall a c t. (Functor t, Foldable t, Hashable a, Ord a, Ord c, Num c)
      => (a -> t (a, c))            -- ^ Neighbor function
      -> (a -> c)                   -- ^ Heuristic function
      -> (a -> Bool)                -- ^ Goal check
      -> a                          -- ^ Starting node
      -> Maybe [a]
rbfs' neighbor heuristic goal root =
    let neighbor' = fmap (fmap (\(a,c) -> (a,a,c))) neighbor
     in (root :) <$> rbfs neighbor' heuristic goal root
{-# INLINABLE rbfs' #-}

mid :: (a, b, c) -> b
mid (_,x,_) = x
{-# INLINE mid #-}
