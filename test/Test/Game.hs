module Test.Game where

import qualified Game
import qualified Test.HUnit as HU

allTests :: HU.Test
allTests = HU.TestList [
  HU.TestLabel "generateCombinationsTest0" generateCombinationsTest0
  , HU.TestLabel "generateCombinationsTest1" generateCombinationsTest1
  ]

generateCombinationsTest0 :: HU.Test
generateCombinationsTest0 = HU.TestCase $ do
  let result = Game.generateCombinations [0 .. 2] 2
  HU.assertEqual "" 3 $ length result
  HU.assertBool "" (elem [0, 1] result)
  HU.assertBool "" (elem [0, 2] result)
  HU.assertBool "" (elem [1, 2] result)

generateCombinationsTest1 :: HU.Test
generateCombinationsTest1 = HU.TestCase $ do
  let result = Game.generateCombinations [0 .. 2] 0
  HU.assertEqual "" 1 $ length result
  HU.assertBool "" (elem [] result)
