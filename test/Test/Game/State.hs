module Test.Game.State (tests) where

import Game.Core qualified as Game
import Game.State (initialRummikubState)
import Game.State qualified as Game
import Relude
import Test.HUnit.Extra (assertLeft)
import Test.Hspec (SpecWith, describe, it)

tests :: SpecWith ()
tests = do
  describe "Game.State" $ do
    describe "modifyTable" $ do
      it "prevents invalid state" $ do
        void $ assertLeft $ Game.modifyTable 3 (Game.ValueTile (1, Game.Red)) initialRummikubState
    describe "modifyRack" $ do
      it "prevents invalid state" $ do
        void $ assertLeft $ Game.modifyRack 3 (Game.ValueTile (1, Game.Red)) initialRummikubState
