module Test.Closed.Extra (tests) where

import Closed.Extra (closedEither)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Closed.Extra" $ do
    describe "closedEither" $ do
      it "works if value within bounds" $ do
        closedEither @1 @10 5 `shouldBe` Right 5

      it "returns a pretty exception" $ do
        closedEither @1 @10 11 `shouldBe` Left "Provided value is out of bounds (from 1 to 10)"
