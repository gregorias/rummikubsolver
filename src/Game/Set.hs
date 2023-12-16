module Game.Set (
  Set,
) where

import Game.Core (Tile (..))

-- | A valid set of tiles.
type Set = [Tile]
