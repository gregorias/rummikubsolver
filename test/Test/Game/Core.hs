{-# LANGUAGE NoImplicitPrelude #-}

module Test.Game.Core (tests) where

import Data.HashSet qualified as HashSet
import Game.Core (Tile)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Game.Core" $ do
    describe "Tile" $ do
      describe "Enum" $ do
        it "assigns bounds to correct integers" $ do
          fromEnum (minBound @Tile) `shouldBe` 0
          -- 13 possible values * 4 possible colors = 52,
          -- + 1 for the joker,
          -- but we start from 0, so -1.
          -- We end up with 52.
          fromEnum (maxBound @Tile) `shouldBe` 52

        it "all enum ints are surjective" $ do
          let (tileList :: [Tile]) = toEnum <$> [0 .. 52]
          let (tileSet :: HashSet Tile) = fromList tileList
          HashSet.size tileSet `shouldBe` 53
