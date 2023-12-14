import GameTest
import Test.Closed.Extra qualified
import Test.HUnit qualified as HU
import Test.Hspec (hspec)

main :: IO ()
main = HU.runTestTT allTests >> hspec Test.Closed.Extra.tests
