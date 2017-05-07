module Data.Serach.Forward.RBFS
(
)
where

rbfs :: forall a c t. (Foldable t, Ord a, Ord c, Num c)
     => (a -> t (a, c))            -- ^ Neighbor function
     -> (a -> c)                   -- ^ Heuristic function
     -> (a -> Bool)                -- ^ Goal check
     -> a                          -- ^ Starting node
     -> Maybe [a]
rbfs neighbor heuristic goal root = undefined
    where go :: a -> Maybe c -> Either (Maybe c) [a]
          go x fLimit
              | goal x = Right [x]
              | otherwise =
                  let xs = neighbor x
                   in case xs of
                          []  -> Left Nothing
                          xs' -> undefined
