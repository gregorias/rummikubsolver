-- | Extra utilities for working with Closed n m.
module Closed.Extra (closedEither) where

import Closed (Closed, closed)
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeNats (KnownNat, type (<=))

-- | A variant of 'Closed.closed' that returns an 'Either' instead of 'Maybe'.
closedEither :: forall n m. (n <= m, KnownNat n, KnownNat m) => Integer -> Either Text (Closed n m)
closedEither =
  maybe
    ( Left $
        "Provided value is out of bounds (from "
          <> showClosed (minBound @(Closed n m))
          <> " to "
          <> showClosed (maxBound @(Closed n m))
          <> ")"
    )
    Right
    . closed
 where
  showClosed = fromString . show @Int . fromIntegral
