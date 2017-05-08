{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Search.Forward.RBFS
(
    rbfs
)
where

import Data.Foldable
import Data.Hashable
import Data.Maybe
import qualified Data.HashPSQ as Q

data Inf a = Inf | Fin !a
    deriving (Show, Eq)

instance Ord a => Ord (Inf a) where
    Inf   <= _     = False
    _     <= Inf   = True
    Fin a <= Fin b = a <= b

rbfs :: forall a c t. (Foldable t, Hashable a, Ord a, Ord c, Num c)
     => (a -> t (a, c))            -- ^ Neighbor function
     -> (a -> c)                   -- ^ Heuristic function
     -> (a -> Bool)                -- ^ Goal check
     -> a                          -- ^ Starting node
     -> Maybe [a]
rbfs neighbor heuristic goal root = 
    case go root Inf (Fin $ heuristic root, 0) of
        Left  _ -> Nothing
        Right x -> Just x

    where go :: a -> Inf c -> (Inf c, c) -> Either (Inf c) [a]
          go x fLimit (f, g)
              | goal x = Right [x]
              | otherwise =
                  let xs = Q.fromList . map (buildS f g)
                         . toList . neighbor $ x
                   in fmap (x :) $ loop fLimit xs
          
          buildS f g (a, c) =
              let g' = g + c
                  f' = max (Fin $ g' + heuristic a) f
               in (a, f', g')

          loop _ (Q.minView -> Nothing) = Left Inf
          loop fLimit (Q.minView -> Just (x, f, g, q))
              | f > fLimit = Left f
              | otherwise =
                  let alt = fromMaybe Inf . fmap mid . Q.findMin $ q
                   in case go x (min fLimit alt) (f, g) of
                          Left f' -> loop fLimit (Q.insert x f' g q)
                          Right z -> Right z

          loop _ _ = error "impossible"

mid :: (a, b, c) -> b
mid (_,x,_) = x
{-# INLINE mid #-}
