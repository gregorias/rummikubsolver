module Test.Interface.TileChangeCommand (tests) where

import Data.HashSet qualified as HashSet
import Game.Core (Color (..), Tile (..))
import Interface.TileChangeCommand (TileChangeCommand (..), parseTileChangeCommand)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Interface.TileChangeCommand" $ do
    describe "parseTileChangeCommand" $ do
      it "parses r13" $ do
        parseTileChangeCommand "r13"
          `shouldBe` TileChangeCommand
            { tccRemove = []
            , tccAdd = [ValueTile (13, Red)]
            }

      it "parses -yl9-10,j,b4" $ do
        let tcc = parseTileChangeCommand "-yl9-10,j,b4"
        HashSet.fromList tcc.tccRemove
          `shouldBe` HashSet.fromList
            [ ValueTile (9, Yellow)
            , ValueTile (10, Yellow)
            , ValueTile (9, Blue)
            , ValueTile (10, Blue)
            ]
        HashSet.fromList tcc.tccAdd `shouldBe` HashSet.fromList [Joker, ValueTile (4, Black)]
