module Test.Interface.TileChangeCommand (tests) where

import Data.HashSet qualified as HashSet
import Game.Core (Color (..), Tile (..))
import Interface.TileChangeCommand (
  TileChangeCommand (..),
  parseTileChangeCommands,
 )
import Relude
import Test.HUnit.Extra (
  assertLeft,
  assertRightOrFailPrint,
  textShouldContain,
 )
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Interface.TileChangeCommand" $ do
    describe "parseTileChangeCommands" $ do
      it "parses r13" $ do
        parseTileChangeCommands "r13"
          `shouldBe` Right [Add (ValueTile (13, Red))]

      it "parses -yl9-10,j,b4" $ do
        let tccsMaybe = parseTileChangeCommands "-yl9-10,j,b4"
        tccs <- assertRightOrFailPrint tccsMaybe
        HashSet.fromList tccs
          `shouldBe` HashSet.fromList
            [ Remove $ ValueTile (9, Yellow)
            , Remove $ ValueTile (10, Yellow)
            , Remove $ ValueTile (9, Blue)
            , Remove $ ValueTile (10, Blue)
            , Add Joker
            , Add $ ValueTile (4, Black)
            ]

      it "doesn't parse garbage" $ do
        errorMessage <- assertLeft $ parseTileChangeCommands "foobar"
        errorMessage `textShouldContain` "expecting tile change commands"
