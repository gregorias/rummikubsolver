module Game.TileCountArray (
  TileCountArray,
  empty,
  toRawArray,
  toElemList,
  tileCount,
  addCount,
  addCountUnsafe,
  union,
) where

import Data.Array.Base (
  accum,
  assocs,
  listArray,
  (!),
 )
import Data.Array.Unboxed (UArray)
import Game.Core (Tile)
import Relude hiding (empty)

-- | An array of tile counts.
--
-- Guarantees that each count is nonnegative.
--
-- Uses a tile's enum value as the index.
newtype TileCountArray = TileCountArray (UArray Int Int)
  deriving stock (Eq, Ord, Show)

empty :: TileCountArray
empty = TileCountArray (listArray (0, fromEnum (maxBound :: Tile)) (repeat 0))

tileCount :: Tile -> TileCountArray -> Int
tileCount t (TileCountArray a) = a ! fromEnum t

toRawArray :: TileCountArray -> UArray Int Int
toRawArray = coerce

toElemList :: TileCountArray -> [Tile]
toElemList (TileCountArray a) =
  concatMap (\(i, c) -> replicate c (toEnum i)) (assocs a)

addCount ::
  Int ->
  Tile ->
  TileCountArray ->
  Either Text TileCountArray
addCount count tile tca = do
  let newTca = addCountUnsafe count tile tca
  let newCount = tileCount tile newTca
  when (newCount < 0) $ Left ("tile count for " <> show tile <> " is negative")
  when (newCount > 2) $ Left ("tile count for " <> show tile <> " is greater than 2")
  return newTca

addCountUnsafe ::
  Int ->
  Tile ->
  TileCountArray ->
  TileCountArray
addCountUnsafe count tile (TileCountArray a) = TileCountArray $ accum (+) a [(fromEnum tile, count)]

union :: TileCountArray -> TileCountArray -> Either Text TileCountArray
union (TileCountArray a) (TileCountArray b) = do
  let resultUnsafe = TileCountArray $ accum (+) a (assocs b)
  let errors =
        assocs (toRawArray resultUnsafe) <&> \(tIdx, count) -> do
          when
            (count > 2)
            ( Left
                $ "tile count for "
                <> show (toEnum @Tile tIdx)
                <> " is greater than 2"
            )
  sequence_ errors
  return resultUnsafe
