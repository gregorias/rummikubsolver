import Relude
import Test.Closed.Extra qualified
import Test.Combinatorics qualified
import Test.Game qualified
import Test.Game.Core qualified
import Test.Game.Set qualified
import Test.Game.State qualified
import Test.Hspec (hspec)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Interface.TileChangeCommand qualified
import Test.Solver qualified

main :: IO ()
main = hspec $ do
  fromHUnitTest Test.Game.huTests
  Test.Closed.Extra.tests
  Test.Combinatorics.tests
  Test.Game.Core.tests
  Test.Game.Set.tests
  Test.Game.State.tests
  Test.Interface.TileChangeCommand.tests
  Test.Solver.tests
