import Test.Closed.Extra qualified
import Test.Game qualified
import Test.Game.Core qualified
import Test.Hspec (hspec)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

main :: IO ()
main = hspec $ do
  fromHUnitTest Test.Game.tests
  Test.Closed.Extra.tests
  Test.Game.Core.tests
