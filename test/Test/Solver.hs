module Test.Solver (tests) where

import Control.Monad
import Data.Maybe
import Game.Core qualified as Game
import Game.TileCountArray qualified as Tca
import Relude
import Solver (solveTable)
import Test.HUnit.Extra (assertJust)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

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
        void $ assertJust $ solveTable table

      it "marks a table as invalid (not all tiles are in sets)" $ do
        let table =
              Tca.empty
                & Tca.addCountUnsafe 1 (Game.ValueTile (1, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (2, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (3, Game.Red))
                & Tca.addCountUnsafe 1 (Game.ValueTile (5, Game.Red))
        solveTable table `shouldBe` Nothing
