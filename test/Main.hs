import qualified Test.HUnit as HU

import Test.Game

main :: IO ()
main = HU.runTestTT allTests >> return ()
