{-# LANGUAGE TupleSections #-}

{-
Module : Game
Description : Data and functions for describing and solving a Rummikub game.
-}
module Game (
    Color (..),
    Tile (..),
    TileArray,
    RummikubState (..),
    minValue,
    maxValue,
    initialRummikubState,
    tileArrayElems,
    modifyTileCountMay,
    modifyTable,
    modifyTableMay,
    modifyRack,
    modifyRackMay,
    solveRummikubState,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.LinearProgram (
    LP (..),
    LPT,
    MsgLev (MsgOff),
    VarKind (IntVar),
    equalTo,
    evalLPT,
    glpSolve,
    mipDefaults,
    msgLev,
    setDirection,
    setObjective,
    setVarKind,
    varBds,
    varLeq,
 )
import Data.LinearProgram.Common (Direction (..))
import Data.Map.Lazy (Map, fromList, union)
import Data.Map.Lazy qualified as Data.Map

-- | Generate all combinations without repetition of given length.
generateCombinations ::
    -- | source list
    [a] ->
    -- | size of combinations
    Int ->
    -- | possible combinations
    [[a]]
generateCombinations [] n
    | n == 0 = [[]]
    | otherwise = []
generateCombinations (s : ss) n
    | n < 0 = []
    | n == 0 = [[]]
    | otherwise =
        map (s :) (generateCombinations ss (n - 1))
            ++ generateCombinations ss n

data Color = Red | Blue | Yellow | Black deriving stock (Bounded, Enum, Eq, Show)

data Tile = ValueTile (Int, Color) | Joker deriving stock (Eq, Show)

instance Bounded Tile where
    minBound = ValueTile (minValue, minBound)

    maxBound = Joker

instance Enum Tile where
    fromEnum (ValueTile (v, c)) = fromEnum c * maxValue + v - 1
    fromEnum Joker = 4 * maxValue

    toEnum n
        | n < 0 || n > 4 * maxValue = undefined
        | n == 4 * maxValue = Joker
        | otherwise = ValueTile (v, c)
      where
        v = n `mod` maxValue + 1
        c = toEnum $ n `div` maxValue

type Set = [Tile]

minValue :: Int
minValue = 1

maxValue :: Int
maxValue = 13

allValues :: [Int]
allValues = [minValue .. maxValue]

minJoker :: Int
minJoker = 0

maxJoker :: Int
maxJoker = 2

allColors :: [Color]
allColors = enumFrom $ toEnum 0

allSets :: [Set]
allSets = seqSets ++ colorSets
  where
    seqSets =
        concat
            [ generateAllSeqSets c j s
            | c <- allColors
            , j <- [minJoker .. maxJoker]
            , s <- [3 .. 5]
            ]
    colorSets =
        concat $
            [generateAllColorSets j 4 | j <- [minJoker .. maxJoker]]
            ++ [generateAllColorSets j 3 | j <- [minJoker .. (maxJoker - 1)]]

generateAllSeqSets :: Color -> Int -> Int -> [[Tile]]
generateAllSeqSets color jokerCount setSize =
    concatMap
        generateSetsFrom
        [minValue .. (maxValue - setSize + jokerCount + 1)]
  where
    valuesToTiles = map (\value -> ValueTile (value, color))
    generateSetsFrom :: Int -> [[Tile]]
    generateSetsFrom begVal =
        map
            (((++) (replicate jokerCount Joker) . valuesToTiles) . (begVal :))
            (generateCombinations [(begVal + 1) .. maxIntervalValue] (setSize - jokerCount - 1))
      where
        maxIntervalValue = min maxValue $ begVal + setSize - 1

generateAllColorSets :: Int -> Int -> [[Tile]]
generateAllColorSets jokerCount setSize = do
    colorComb <- colorCombs
    value <- allValues
    return $ jokers ++ map (\c -> ValueTile (value, c)) colorComb
  where
    colorCombs = generateCombinations allColors $ setSize - jokerCount
    jokers = replicate jokerCount Joker

initSParameters :: (IArray a Int) => [Set] -> a (Int, Int) Int
initSParameters sets =
    accumArray (\count _ -> count + 1) 0 sArrayBounds assocList
  where
    setToIndexes = map fromEnum
    setsWithIndexes = zip [0 ..] $ map setToIndexes sets
    arrayIndexes :: [(Int, Int)]
    arrayIndexes = concatMap (\(v, set) -> map (v,) set) setsWithIndexes
    assocList = map (,undefined) arrayIndexes
    setCount = length sets
    sArrayBounds = ((0, 0), (setCount, fromEnum (maxBound :: Tile)))

initModel ::
    (IArray a Int, MonadState (Data.LinearProgram.LP (Int, Int) Int) m) =>
    a (Int, Int) Int ->
    a Int Int ->
    a Int Int ->
    m ()
initModel sArr tableArg rackArg = do
    setDirection Max
    setObjective $ fromList $ map (curry (,1) 0) [0 .. tileSize]
    variableKinds setSize
    setVariableBounds setSize
    rackVariableBounds rackArg
    tileConstraints sArr tableArg
  where
    sBounds = bounds sArr
    tileSize = (snd . snd) sBounds
    setSize = (fst . snd) sBounds

setVariableBounds ::
    (MonadState (Data.LinearProgram.LP (Int, Int) Int) m) =>
    Int ->
    m ()
setVariableBounds setSize =
    mapM_ (\i -> varBds (1, i) 0 2) [0 .. setSize]

rackVariableBounds ::
    (MonadState (Data.LinearProgram.LP (Int, Int) Int) m, IArray a Int) =>
    a Int Int ->
    m ()
rackVariableBounds rackArg =
    mapM_ (\i -> varLeq (0, i) (rackArg ! i)) [0 .. rackSize]
  where
    rackSize = fromEnum (maxBound :: Tile)

variableKinds ::
    (MonadState (Data.LinearProgram.LP (Int, Int) Int) m) =>
    Int ->
    m ()
variableKinds setSize = do
    mapM_ (\i -> setVarKind (1, i) IntVar) [0 .. setSize]
    mapM_ (\i -> setVarKind (0, i) IntVar) [0 .. rackSize]
  where
    rackSize = fromEnum (maxBound :: Tile)

tileConstraints ::
    (IArray a Int, MonadState (Data.LinearProgram.LP (Int, Int) Int) m) =>
    a (Int, Int) Int ->
    a Int Int ->
    m ()
tileConstraints sArr tableArg =
    mapM_ createTileConstraint [0 .. 52]
  where
    setSize = fst . snd . bounds $ sArr
    createTileConstraint tileIndex =
        equalTo
            (tileUsedInSetsCombination `union` tilesPlacedFromRack)
            (tableArg ! tileIndex)
      where
        tileUsedInSetsCombination :: Map (Int, Int) Int
        tileUsedInSetsCombination =
            fromList $
                map
                    (\setIndex -> ((1, setIndex), sArr ! (setIndex, tileIndex)))
                    [0 .. setSize]
        tilesPlacedFromRack :: Map (Int, Int) Int
        tilesPlacedFromRack = fromList [((0, tileIndex), -1)]

solveModel ::
    (MonadIO m) =>
    Data.LinearProgram.LPT (Int, Int) Int m () ->
    m (Maybe ([Set], [Tile]))
solveModel model = do
    result <- Data.LinearProgram.evalLPT $ model >> glpSolve (mipDefaults{msgLev = MsgOff})
    let maybeVarMap = fmap snd . snd $ result
    return $ fmap varMapToResult maybeVarMap
  where
    varMapToResult :: Map (Int, Int) Double -> ([Set], [Tile])
    varMapToResult =
        Data.Map.foldrWithKey
            ( \(x, y) value (sets, tiles) ->
                if x == 0
                    then (sets, replicate (round value) (toEnum y) ++ tiles)
                    else (replicate (round value) (setIdxToSet y) ++ sets, tiles)
            )
            ([], [])
            . Data.Map.filter (> 0)
    setIdxToSet = (!!) allSets

--- Game State
type TileArray = UArray Int Int

data RummikubState = RummikubState
    { table :: TileArray
    , rack :: TileArray
    }
    deriving stock (Eq, Show)

initialRummikubState :: RummikubState
initialRummikubState =
    RummikubState
        (array arrayBounds $ map (,0) [0 .. (snd arrayBounds)])
        (array arrayBounds $ map (,0) [0 .. (snd arrayBounds)])
  where
    arrayBounds = (0, fromEnum (maxBound :: Tile))

tileArrayElems :: TileArray -> [Tile]
tileArrayElems tileArray =
    concatMap (\(i, c) -> replicate c (toEnum i)) (assocs tileArray)

modifyTileCount ::
    Int ->
    Tile ->
    TileArray ->
    TileArray
modifyTileCount count tile tileArray =
    accum
        (+)
        tileArray
        [(fromEnum tile, count)]

isTileArrayConsistent :: TileArray -> Bool
isTileArrayConsistent = all ((&&) <$> (<= 2) <*> (>= 0)) . elems

isRummikubStateConsistent :: RummikubState -> Bool
isRummikubStateConsistent state =
    isTileArrayConsistent tableArray
        && isTileArrayConsistent rackArray
        && isTileArrayConsistent combinedArrays
  where
    tableArray = table state
    rackArray = rack state
    combinedArrays = accum (+) tableArray (assocs rackArray)

modifyTileCountMay ::
    Int ->
    Tile ->
    TileArray ->
    Maybe TileArray
modifyTileCountMay count tile tileArray =
    if isTileArrayConsistent result
        then Just result
        else Nothing
  where
    result = modifyTileCount count tile tileArray

modifyTable ::
    Int ->
    Tile ->
    RummikubState ->
    RummikubState
modifyTable count tile state =
    RummikubState (modifyTileCount count tile $ table state) (rack state)

modifyTableMay ::
    Int ->
    Tile ->
    RummikubState ->
    Maybe RummikubState
modifyTableMay count tile state =
    if isRummikubStateConsistent newState
        then Just newState
        else Nothing
  where
    newState =
        RummikubState
            (modifyTileCount count tile $ table state)
            (rack state)

modifyRack ::
    Int ->
    Tile ->
    RummikubState ->
    RummikubState
modifyRack count tile state =
    RummikubState (table state) (modifyTileCount count tile $ rack state)

modifyRackMay ::
    Int ->
    Tile ->
    RummikubState ->
    Maybe RummikubState
modifyRackMay count tile state =
    if isRummikubStateConsistent newState
        then Just newState
        else Nothing
  where
    newState =
        RummikubState
            (table state)
            (modifyTileCount count tile $ rack state)

solveRummikubState :: RummikubState -> IO (Maybe ([Set], [Tile]))
solveRummikubState state =
    let sArr = initSParameters allSets
        model = initModel sArr (table state) (rack state)
     in solveModel model
