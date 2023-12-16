{-# LANGUAGE NoImplicitPrelude #-}

module Test.Combinatorics (
  tests,
) where

import Combinatorics (generateCombinations)
import Data.HashSet qualified as HashSet
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = describe "Combinatorics" $ do
  describe "generateCombinations" $ do
    it "generates no combinations from an empty list" $ do
      generateCombinations @Int [] 1 `shouldBe` []

    it "generates no combinations if size too big" $ do
      generateCombinations [0 .. 2 :: Int] 4 `shouldBe` []

    it "generates an empty combination from size 0" $ do
      generateCombinations [0 .. 2 :: Int] 0 `shouldBe` [[]]

    it "generates all combinations of size 2" $ do
      let result = generateCombinations [0 .. 2 :: Int] 2
      HashSet.fromList result `shouldBe` HashSet.fromList [[0, 1], [0, 2], [1, 2]]
