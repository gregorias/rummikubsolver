module Data.List.Extra (
  partition,
  PartitionSelect (..),
) where

import Relude (foldr)

-- | A better partition function.
partition :: (a -> PartitionSelect l r) -> [a] -> ([l], [r])
partition f = foldr (select f) ([], [])
 where
  select f x ~(ls, rs) = case f x of
    L y -> (y : ls, rs)
    R y -> (ls, y : rs)

data PartitionSelect l r = L l | R r
