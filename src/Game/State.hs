-- | The game state from a solver's perspective.
module Game.State (
  RummikubState,
  initialRummikubState,
  table,
  rack,
  modifyTable,
  modifyRack,
) where

import Data.Foldable (foldrM)
import Game.Core (Tile)
import Game.TileCountArray (TileCountArray, addCount)
import Game.TileCountArray qualified as TileCountArray
import Relude
import Solver (solveTable)

-- | The game state from a solver's perspective.
--
-- Guarantees that:
-- - Tile counts are consistent with Rummikub's rules.
-- - The table always has only tiles present in sets.
data RummikubState = RummikubState
  { table :: TileCountArray
  , rack :: TileCountArray
  }
  deriving stock (Eq, Show)

initialRummikubState :: RummikubState
initialRummikubState =
  RummikubState TileCountArray.empty TileCountArray.empty

isRummikubStateConsistent :: RummikubState -> Either Text ()
isRummikubStateConsistent state = do
  void $ TileCountArray.union state.table state.rack
  case solveTable state.table of
    Nothing -> Left "the table is not solvable"
    Just _ -> return ()

modifyTable :: [(Int, Tile)] -> RummikubState -> Either Text RummikubState
modifyTable changes state = do
  newTable <- foldrM (uncurry addCount) state.table changes
  let newState = RummikubState newTable state.rack
  isRummikubStateConsistent newState
  return newState

modifyRack :: Int -> Tile -> RummikubState -> Either Text RummikubState
modifyRack count tile state = do
  newRack <- addCount count tile state.rack
  let newState = RummikubState state.table newRack
  isRummikubStateConsistent newState
  return newState
