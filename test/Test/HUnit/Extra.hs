-- | Extra functionalities for HUnit.
module Test.HUnit.Extra (assertJust, assertRightOrFailPrint) where

import Relude
import Test.HUnit (assertFailure)

-- | Asserts that the given Maybe is Just.
assertJust :: Maybe a -> IO a
assertJust (Just a) = return a
assertJust Nothing = assertFailure "Expected Just, got Nothing"

-- | Asserts that the given Either is Right.
--
-- Prints the error message if not.
-- Better than just assertRight, because it maintains newline formatting.
assertRightOrFailPrint :: Either Text r -> IO r
assertRightOrFailPrint (Left err) = assertFailure . toString $ "Expected Right, got an error:\n" <> err
assertRightOrFailPrint (Right r) = return r
