module Game.TileCountArray (
  TileCountArray,
  empty,
  toRawArray,
  toElemList,
  tileCount,
  addCount,
) where

import Data.Array.Base (accum, assocs, listArray, (!))
import Data.Array.Unboxed (UArray)
import Game.Core (Tile)
import Relude hiding (empty)

-- | An array of tile countes.
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
  Maybe TileCountArray
addCount count tile tca@(TileCountArray a) = do
  guard $ (a ! fromEnum tile) + count >= 0
  return $ addCountUnsafe count tile tca

addCountUnsafe ::
  Int ->
  Tile ->
  TileCountArray ->
  TileCountArray
addCountUnsafe count tile (TileCountArray a) = TileCountArray $ accum (+) a [(fromEnum tile, count)]
