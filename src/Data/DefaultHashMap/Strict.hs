{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A hash map with default values.
--
-- A speed implementation just for convenience of having a default of 0 for representing counts.
module Data.DefaultHashMap.Strict (
  DefaultHashMap,
  insertedEntries,

  -- * Construction
  empty,

  -- * Basic interface
  lookup,
  (!),
  insert,
  adjust,

  -- * Combine

  -- ** Union
  union,
  unionWith,
) where

import Data.HashMap.Strict qualified as HM
import Relude (Functor (..), Hashable, flip)

data DefaultHashMap k v = DefaultHashMap
  { dhmDefault :: !v
  , dhmMap :: !(HM.HashMap k v)
  }

insertedEntries :: DefaultHashMap k v -> HM.HashMap k v
insertedEntries = dhmMap

instance Functor (DefaultHashMap k) where
  fmap f dhm = dhm{dhmDefault = f dhm.dhmDefault, dhmMap = HM.map f dhm.dhmMap}

empty :: v -> DefaultHashMap k v
empty v = DefaultHashMap v HM.empty

lookup :: (Hashable k) => k -> DefaultHashMap k v -> v
lookup k dhm = HM.findWithDefault dhm.dhmDefault k dhm.dhmMap

(!) :: (Hashable k) => DefaultHashMap k v -> k -> v
(!) = flip lookup

infixl 9 !

insert :: (Hashable k) => k -> v -> DefaultHashMap k v -> DefaultHashMap k v
insert k v dhm = dhm{dhmMap = HM.insert k v dhm.dhmMap}

adjust :: (Hashable k) => (v -> v) -> k -> DefaultHashMap k v -> DefaultHashMap k v
adjust f k dhm =
  let v = lookup k dhm
   in insert k (f v) dhm

-- Always uses the left default value.
union :: (Hashable k) => DefaultHashMap k v -> DefaultHashMap k v -> DefaultHashMap k v
union dhml dhmr = dhml{dhmMap = HM.union dhml.dhmMap dhmr.dhmMap}

-- Always uses the left default value.
unionWith ::
  (Hashable k) =>
  (v -> v -> v) ->
  DefaultHashMap k v ->
  DefaultHashMap k v ->
  DefaultHashMap k v
unionWith op dhml dhmr = dhml{dhmMap = HM.unionWith op dhml.dhmMap dhmr.dhmMap}
