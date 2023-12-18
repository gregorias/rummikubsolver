-- | The game state from a solver's perspective.
module Game.State (
  RummikubState,
  initialRummikubState,
  table,
  rack,
  modifyTable,
  modifyRack,
) where

import Game.Core (Tile)
import Game.TileCountArray (TileCountArray, addCount)
import Game.TileCountArray qualified as TileCountArray
import Relude

-- | The game state from a solver's perspective.
--
-- Guarantees that tile counts are consistent with Rummikub's rules.
data RummikubState = RummikubState
  { table :: TileCountArray
  , rack :: TileCountArray
  }
  deriving stock (Eq, Show)

initialRummikubState :: RummikubState
initialRummikubState =
  RummikubState TileCountArray.empty TileCountArray.empty

isRummikubStateConsistent :: RummikubState -> Bool
isRummikubStateConsistent state = isJust $ TileCountArray.union state.table state.rack

modifyTable :: Int -> Tile -> RummikubState -> Maybe RummikubState
modifyTable count tile state = do
  newTable <- addCount count tile state.table
  let newState = RummikubState newTable state.rack
  guard $ isRummikubStateConsistent newState
  return newState

modifyRack :: Int -> Tile -> RummikubState -> Maybe RummikubState
modifyRack count tile state = do
  newRack <- addCount count tile state.rack
  let newState = RummikubState state.table newRack
  guard $ isRummikubStateConsistent newState
  return newState
