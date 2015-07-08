import qualified Test.HUnit as HU

import GameTest

main :: IO ()
main = HU.runTestTT allTests >> return ()
