import Relude
import Test.Closed.Extra qualified
import Test.Combinatorics qualified
import Test.Game qualified
import Test.Game.Core qualified
import Test.Game.Set qualified
import Test.Hspec (hspec)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Interface.TileChangeCommand qualified

main :: IO ()
main = hspec $ do
  fromHUnitTest Test.Game.tests
  Test.Closed.Extra.tests
  Test.Combinatorics.tests
  Test.Game.Core.tests
  Test.Game.Set.tests
  Test.Interface.TileChangeCommand.tests
