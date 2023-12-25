{-# LANGUAGE OverloadedRecordDot #-}

{-
Module : Game
Description : Data and functions for solving a Rummikub game.
-}
module Game (
  solveRummikubState,
) where

import Game.Core (Tile)
import Game.Set (Set)
import Game.State (RummikubState (..))
import Relude hiding (Set)
import Solver (solveTableAndRack)

solveRummikubState :: RummikubState -> IO (Maybe ([Set], [Tile]))
solveRummikubState state =
  solveTableAndRack state.table state.rack
