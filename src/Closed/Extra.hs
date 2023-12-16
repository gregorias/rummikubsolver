-- | Extra utilities for working with Closed n m.
module Closed.Extra (closedEither) where

import Closed (Closed, closed)
import GHC.TypeNats (type (<=))
import Relude

-- | A variant of 'Closed.closed' that returns an 'Either' instead of 'Maybe'.
closedEither :: forall n m. (n <= m, KnownNat n, KnownNat m) => Integer -> Either Text (Closed n m)
closedEither =
  maybe
    ( Left
        $ "Provided value is out of bounds (from "
        <> showClosed (minBound @(Closed n m))
        <> " to "
        <> showClosed (maxBound @(Closed n m))
        <> ")"
    )
    Right
    . closed
 where
  showClosed = (show @_ @Int) . fromIntegral
