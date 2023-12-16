module Test.Game.Set (tests) where

import Game.Set (allSets)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Game.Set" $ do
    describe "allSets" $ do
      it "has 1173 sets" $ do
        length allSets `shouldBe` 1173
    describe "toList" $ do
      it "returns a set in natural order" $ do
        length allSets `shouldBe` 1173
