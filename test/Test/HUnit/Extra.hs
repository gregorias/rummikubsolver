-- | Extra functionalities for HUnit.
module Test.HUnit.Extra (assertJust) where

import Relude
import Test.HUnit (assertFailure)

-- | Asserts that the given Maybe is Just.
assertJust :: Maybe a -> IO a
assertJust (Just a) = return a
assertJust Nothing = assertFailure "Expected Just, got Nothing"
