module Test.Game.State (tests) where

import Game.Core qualified as Game
import Game.State (initialRummikubState)
import Game.State qualified as Game
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Game.State" $ do
    describe "modifyTable" $ do
      it "prevents invalid state" $ do
        Game.modifyTable 3 (Game.ValueTile (1, Game.Red)) initialRummikubState
          `shouldBe` Nothing
    describe "modifyRack" $ do
      it "prevents invalid state" $ do
        Game.modifyRack 3 (Game.ValueTile (1, Game.Red)) initialRummikubState
          `shouldBe` Nothing
