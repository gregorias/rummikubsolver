{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Game (
  huTests,
  tests,
) where

import Control.Monad
import Data.Maybe
import Game qualified
import Game.Core qualified as Game
import Game.State qualified as Game
import Game.TileCountArray qualified as Tca
import Relude
import Test.HUnit qualified as HU
import Test.HUnit.Extra (assertJust, assertRightOrFailPrint)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

huTests :: HU.Test
huTests =
  HU.TestList
    [ HU.TestLabel
        "shouldSolveTheRummikubCompletely"
        shouldSolveTheRummikubCompletely
    ]

shouldSolveTheRummikubCompletely :: HU.Test
shouldSolveTheRummikubCompletely = HU.TestCase $ do
  rummikubState <- assertRightOrFailPrint rummikubStateMay
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
    foldr ((<=<) . Game.modifyRack 1) Right tilesOnRack
      <=< foldr ((<=<) . Game.modifyTable 1) Right tilesOnTable
        $ Game.initialRummikubState

tests :: SpecWith ()
tests = do
  describe "Game" $ do
    describe "solveTable" $ do
      it "solves a table" $ do
        let table =
              Tca.empty
                & Tca.addCountUnsafe 1 (Game.ValueTile (1, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (2, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (3, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (4, Game.Red))
        void $ assertJust $ Game.solveTable table

      it "marks a table as invalid (not all tiles are in sets)" $ do
        let table =
              Tca.empty
                & Tca.addCountUnsafe 1 (Game.ValueTile (1, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (2, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (3, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (5, Game.Red))
        Game.solveTable table `shouldBe` Nothing
