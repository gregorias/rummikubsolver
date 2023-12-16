{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Game (
  tests,
) where

import Control.Monad
import Data.Maybe
import Game qualified
import Game.Core qualified as Game
import Test.HUnit qualified as HU

tests :: HU.Test
tests =
  HU.TestList
    [ HU.TestLabel
        "shouldReturnNothingOnInvalidMoves0"
        shouldReturnNothingOnInvalidMoves0
    , HU.TestLabel
        "shouldReturnNothingOnInvalidMoves1"
        shouldReturnNothingOnInvalidMoves1
    , HU.TestLabel
        "shouldSolveTheRummikubCompletely"
        shouldSolveTheRummikubCompletely
    ]

shouldReturnNothingOnInvalidMoves0 :: HU.Test
shouldReturnNothingOnInvalidMoves0 =
  Nothing
    HU.~=? Game.modifyTableMay
      3
      (Game.ValueTile (1, Game.Red))
      Game.initialRummikubState

shouldReturnNothingOnInvalidMoves1 :: HU.Test
shouldReturnNothingOnInvalidMoves1 =
  Nothing
    HU.~=? Game.modifyRackMay
      3
      (Game.ValueTile (1, Game.Red))
      Game.initialRummikubState

shouldSolveTheRummikubCompletely :: HU.Test
shouldSolveTheRummikubCompletely = HU.TestCase $ do
  HU.assertBool "" $ isJust rummikubStateMay
  let Just rummikubState = rummikubStateMay
  solutionMaybe <- Game.solveRummikubState rummikubState
  HU.assertBool "" $ isJust solutionMaybe
  let Just (_, tiles) = solutionMaybe
  HU.assertEqual "" (length tilesOnRack) (length tiles)
 where
  tilesOnTable =
    map (\v -> Game.ValueTile (v, Game.Red)) [5 .. 13]
      ++ map (\v -> Game.ValueTile (v, Game.Blue)) ([7 .. 13] ++ [10, 1])
      ++ map (\v -> Game.ValueTile (v, Game.Yellow)) [1, 10, 11]
      ++ map
        (\v -> Game.ValueTile (v, Game.Black))
        ([3 .. 7] ++ [9 .. 11] ++ [1, 13])
      ++ [Game.Joker]
  tilesOnRack =
    map (\v -> Game.ValueTile (v, Game.Red)) [2, 12]
      ++ [Game.ValueTile (2, Game.Blue)]
      ++ map (\v -> Game.ValueTile (v, Game.Yellow)) [2, 5, 6, 8]
      ++ map (\v -> Game.ValueTile (v, Game.Black)) [2, 12]
  rummikubStateMay =
    foldr ((<=<) . Game.modifyRackMay 1) Just tilesOnRack
      <=< foldr ((<=<) . Game.modifyTableMay 1) Just tilesOnTable
      $ Game.initialRummikubState
