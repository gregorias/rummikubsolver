-- | Core primitives for representing a Rummikub game.
module Game.Core (
  -- * Data types
  Color (..),
  Tile (..),
  Set,
  minValue,
  maxValue,
) where

-- | The color of a tile.
data Color = Red | Blue | Yellow | Black deriving stock (Bounded, Enum, Eq, Show)

-- | A single tile in the game.
data Tile = ValueTile (Int, Color) | Joker deriving stock (Eq, Show)

instance Bounded Tile where
  minBound = ValueTile (minValue, minBound)

  maxBound = Joker

instance Enum Tile where
  fromEnum (ValueTile (v, c)) = fromEnum c * maxValue + v - 1
  fromEnum Joker = 4 * maxValue

  toEnum n
    | n < 0 || n > 4 * maxValue = undefined
    | n == 4 * maxValue = Joker
    | otherwise = ValueTile (v, c)
   where
    v = n `mod` maxValue + 1
    c = toEnum $ n `div` maxValue

type Set = [Tile]

minValue :: Int
minValue = 1

maxValue :: Int
maxValue = 13
