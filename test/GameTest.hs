{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GameTest (
    allTests,
) where

import Control.Monad
import Data.Maybe
import Game qualified
import Test.HUnit qualified as HU

allTests :: HU.Test
allTests =
    HU.TestList
        [ HU.TestLabel "generateCombinationsTest0" generateCombinationsTest0
        , HU.TestLabel "generateCombinationsTest1" generateCombinationsTest1
        , HU.TestLabel "generateCombinationsTest2" generateCombinationsTest2
        , HU.TestLabel "generateCombinationsTest3" generateCombinationsTest3
        , HU.TestLabel "shouldBe1173Sets" shouldBe1173Sets
        , HU.TestLabel
            "shouldReturnNothingOnInvalidMoves0"
            shouldReturnNothingOnInvalidMoves0
        , HU.TestLabel
            "shouldReturnNothingOnInvalidMoves1"
            shouldReturnNothingOnInvalidMoves1
        , HU.TestLabel
            "shouldSolveTheRummikubCompletely"
            shouldSolveTheRummikubCompletely
        ]

generateCombinationsTest0 :: HU.Test
generateCombinationsTest0 = HU.TestCase $ do
    let result = Game.generateCombinations [0 .. 2 :: Int] 2
    HU.assertEqual "" 3 $ length result
    HU.assertBool "" ([0, 1] `elem` result)
    HU.assertBool "" ([0, 2] `elem` result)
    HU.assertBool "" ([1, 2] `elem` result)

generateCombinationsTest1 :: HU.Test
generateCombinationsTest1 = HU.TestCase $ do
    let result = Game.generateCombinations [0 .. 2 :: Int] 0
    HU.assertEqual "" 1 $ length result
    HU.assertBool "" ([] `elem` result)

generateCombinationsTest2 :: HU.Test
generateCombinationsTest2 = HU.TestCase $ HU.assertEqual "" [] $ Game.generateCombinations [0 .. 2 :: Int] 4

generateCombinationsTest3 :: HU.Test
generateCombinationsTest3 = HU.TestCase $ HU.assertEqual "" [] $ Game.generateCombinations [0 .. 2 :: Int] (-1)

shouldBe1173Sets :: HU.Test
shouldBe1173Sets = 1173 HU.~=? length Game.allSets

shouldReturnNothingOnInvalidMoves0 :: HU.Test
shouldReturnNothingOnInvalidMoves0 =
    Nothing
        HU.~=? Game.modifyTableMay
            3
            (Game.ValueTile (1, Game.Red))
            Game.initialRummikubState

shouldReturnNothingOnInvalidMoves1 :: HU.Test
shouldReturnNothingOnInvalidMoves1 =
    Nothing
        HU.~=? Game.modifyRackMay
            3
            (Game.ValueTile (1, Game.Red))
            Game.initialRummikubState

shouldSolveTheRummikubCompletely :: HU.Test
shouldSolveTheRummikubCompletely = HU.TestCase $ do
    HU.assertBool "" $ isJust rummikubStateMay
    let Just rummikubState = rummikubStateMay
    solutionMaybe <- Game.solveRummikubState rummikubState
    HU.assertBool "" $ isJust solutionMaybe
    let Just (_, tiles) = solutionMaybe
    HU.assertEqual "" (length tilesOnRack) (length tiles)
  where
    tilesOnTable =
        map (\v -> Game.ValueTile (v, Game.Red)) [5 .. 13]
            ++ map (\v -> Game.ValueTile (v, Game.Blue)) ([7 .. 13] ++ [10, 1])
            ++ map (\v -> Game.ValueTile (v, Game.Yellow)) [1, 10, 11]
            ++ map
                (\v -> Game.ValueTile (v, Game.Black))
                ([3 .. 7] ++ [9 .. 11] ++ [1, 13])
            ++ [Game.Joker]
    tilesOnRack =
        map (\v -> Game.ValueTile (v, Game.Red)) [2, 12]
            ++ [Game.ValueTile (2, Game.Blue)]
            ++ map (\v -> Game.ValueTile (v, Game.Yellow)) [2, 5, 6, 8]
            ++ map (\v -> Game.ValueTile (v, Game.Black)) [2, 12]
    rummikubStateMay =
        foldr ((<=<) . Game.modifyRackMay 1) Just tilesOnRack
            <=< foldr ((<=<) . Game.modifyTableMay 1) Just tilesOnTable
            $ Game.initialRummikubState
