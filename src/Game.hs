module Game where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Data.Array.IArray
import Data.Array.Unboxed (UArray) 
import Data.LinearProgram
import Data.Map.Lazy (Map, fromList, union)
import qualified Data.Map.Lazy as Data.Map

generateCombinations :: [a] -> Int -> [[a]]
generateCombinations source n | n <= 0 = [[]]
                              | otherwise =
  generateCombinations' source n (length source)
  where
    generateCombinations' (s : ss) n sourceLength
      | sourceLength < n = []
      | otherwise =
        map ((:) s) (generateCombinations' ss (n - 1) (sourceLength - 1))
        ++
        (generateCombinations' ss n (sourceLength - 1))
    generateCombinations' [] 0 0 =  [[]]
    generateCombinations' [] _ 0 =  []

data Color = Red | Blue | Yellow | Black deriving (Bounded, Enum, Eq, Show)

data Tile = ValueTile (Int, Color) | Joker deriving (Eq, Show)

instance Bounded Tile where
  minBound = ValueTile (minValue, minBound)

  maxBound = Joker

instance Enum Tile where
  fromEnum (ValueTile (v, c)) = fromEnum c * maxValue + v - 1
  fromEnum Joker = 4 * maxValue

  toEnum n | n < 0 || n > 4 * maxValue = undefined
           | n == 4 * maxValue = Joker
           | otherwise = ValueTile (v, c)
             where
               v = (n `mod` maxValue) + 1
               c = toEnum $ n `div` maxValue

type Set = [Tile]

minValue :: Int
minValue = 1

maxValue :: Int
maxValue = 13

allValues :: [Int]
allValues = [minValue..maxValue]

minJoker :: Int
minJoker = 0

maxJoker :: Int
maxJoker = 2

allColors :: [Color]
allColors = enumFrom $ toEnum 0

allSets :: [Set]
allSets = seqSets ++ colorSets
  where
    seqSets = concat [generateAllSeqSets c j s |
        c <- allColors, j <- [minJoker..maxJoker], s <- [3..5]]
    colorSets = concat 
      $ [generateAllColorSets j 4 | j <- [minJoker..maxJoker]]
        ++ [generateAllColorSets j 3 | j <- [minJoker..(maxJoker - 1)]]

generateAllSeqSets :: Color -> Int -> Int -> [[Tile]]
generateAllSeqSets color jokerCount setSize = concatMap
  (\v -> generateSetsFrom v)
  [minValue..(maxValue - setSize + jokerCount + 1)]
  where
    valuesToTiles = map (\value -> ValueTile (value, color))
    generateSetsFrom :: Int -> [[Tile]]
    generateSetsFrom begVal = map ((++) (replicate jokerCount Joker))
      $ map valuesToTiles
      $ map (begVal :)
      $ generateCombinations [(begVal + 1)..maxIntervalValue] (setSize - jokerCount - 1) 
      where
        maxIntervalValue = min maxValue $ begVal + setSize - 1

generateAllColorSets :: Int -> Int -> [[Tile]]
generateAllColorSets jokerCount setSize = do
    colorComb <- colorCombs
    value <- allValues
    return $ jokers ++ (map (\c -> ValueTile (value, c)) colorComb)
  where
    colorCombs = generateCombinations allColors $ setSize - jokerCount
    jokers = replicate jokerCount Joker

initSParameters :: (IArray a Int) => [Set] -> a (Int, Int) Int
initSParameters sets =
  accumArray (\count _ -> count + 1) 0 bounds assocList
  where 
    setToIndexes = map fromEnum
    setsWithIndexes = zip [0..] $ map setToIndexes sets
    arrayIndexes :: [(Int, Int)]
    arrayIndexes = concatMap (\(v, set) -> zip (repeat v) set) setsWithIndexes
    assocList = zip arrayIndexes (repeat undefined)
    setCount = length sets
    bounds = ((0, 0), (setCount, fromEnum $ (maxBound :: Tile)))

initModel :: (IArray a Int, MonadState (LP (Int, Int) Int) m)
  => a (Int, Int) Int
  -> a Int Int
  -> a Int Int
  -> m ()
initModel sArr table rack = do
  setDirection Max
  setObjective $ fromList $ zip (zip (repeat 0) [0..tileSize]) (repeat 1)
  variableKinds setSize
  setVariableBounds setSize
  rackVariableBounds rack
  tileConstraints sArr table
  where 
    sBounds = bounds sArr
    tileSize = (snd . snd) sBounds
    setSize = (fst . snd) sBounds
  -- set all variables as integers
  -- set rack bounds (y's) <= rack
  
  -- set tile constraints (x's) sum s[i][j] * x[i] == table[j] + y[j]

setVariableBounds ::
  (MonadState (LP (Int, Int) Int) m)
  => Int
  -> m ()
setVariableBounds setSize = sequence_
  $ map (\i -> varBds (1, i) 0  2) [0 .. setSize]

rackVariableBounds ::
  (MonadState (LP (Int, Int) Int) m, IArray a Int)
  => a Int Int
  -> m()
rackVariableBounds rack = sequence_ 
  $ map (\i -> varLeq (0, i) ( rack ! i)) [0 .. rackSize]
  where
    rackSize = fromEnum $ (maxBound :: Tile)

variableKinds :: 
  (MonadState (LP (Int, Int) Int) m)
  => Int
  -> m ()
variableKinds setSize = do
  sequence_ $ map (\i -> setVarKind (1, i) IntVar) [0 .. setSize]
  sequence_ $ map (\i -> setVarKind (0, i) IntVar) [0 .. rackSize]
  where
    rackSize = fromEnum (maxBound :: Tile)

tileConstraints sArr table = 
  sequence_ $ map createTileConstraint [0..52]
  where 
    setSize = fst . snd . bounds $ sArr
    createTileConstraint tileIndex =
      equalTo
        (union tileUsedInSetsCombination tilesPlacedFromRack)
        (table ! tileIndex)
      where
        tileUsedInSetsCombination :: Map (Int, Int) Int
        tileUsedInSetsCombination = fromList 
          $ map 
            (\setIndex -> ((1, setIndex), (sArr ! (setIndex, tileIndex))))
            [0 .. setSize]
        tilesPlacedFromRack :: Map (Int, Int) Int
        tilesPlacedFromRack = fromList [((0, tileIndex), -1)]


solveModel ::
  (Monad m, MonadIO m)
  => LPT (Int, Int) Int m ()
  -> m (Maybe ([Set], [Tile]))
solveModel model = do
  result <- evalLPT $ model >> quickSolveMIP
  let maybeVarMap = fmap snd . snd $ result
  return $ fmap varMapToResult maybeVarMap
  where 
    varMapToResult :: Map (Int, Int) Double -> ([Set], [Tile])
    varMapToResult = 
      Data.Map.foldrWithKey 
          (\(x, y) (value) (sets, tiles) -> 
            if x == 0
            then (sets, replicate (round value) (toEnum y) ++ tiles)
            else (replicate (round value) (setIdxToSet y) ++ sets, tiles))
          ([], [])
      . Data.Map.filter (> 0)
    setIdxToSet = (!!) allSets

--- Game State
type TileArray = UArray Int Int

data RummikubState = RummikubState {
  table :: TileArray
  , rack :: TileArray
  } deriving (Eq, Show)

initialRummikubState :: RummikubState
initialRummikubState = RummikubState
  (array bounds $ zip [0 .. (snd bounds)] $ repeat 0)
  (array bounds $ zip [0 .. (snd bounds)] $ repeat 0)
  where
    bounds = (0, fromEnum (maxBound :: Tile))

tileArrayElems :: TileArray -> [Tile]
tileArrayElems tileArray = 
  concatMap (\(i, c) -> replicate c (toEnum i)) (assocs tileArray)

modifyTileCount :: Int
  -> Tile
  -> TileArray
  -> TileArray
modifyTileCount count tile array = accum (+) array [(fromEnum tile, count)]

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

modifyTileCountMay :: Int
  -> Tile
  -> TileArray
  -> Maybe TileArray
modifyTileCountMay count tile array =
  if isTileArrayConsistent result
  then Just result
  else Nothing
  where
    result = modifyTileCount count tile array

modifyTable :: Int
  -> Tile
  -> RummikubState
  -> RummikubState
modifyTable count tile state = 
  RummikubState (modifyTileCount count tile $ table state) (rack state)

modifyTableMay :: Int
  -> Tile
  -> RummikubState
  -> Maybe RummikubState
modifyTableMay count tile state = 
  if isRummikubStateConsistent newState
  then Just newState
  else Nothing
  where 
    newState = RummikubState 
      (modifyTileCount count tile $ table state) 
      (rack state)

modifyRack :: Int
  -> Tile
  -> RummikubState
  -> RummikubState
modifyRack count tile state = 
  RummikubState (table state) (modifyTileCount count tile $ rack state)

modifyRackMay :: Int
  -> Tile
  -> RummikubState
  -> Maybe RummikubState
modifyRackMay count tile state = 
  if isRummikubStateConsistent newState
  then Just newState
  else Nothing
  where 
    newState = RummikubState 
      (table state) 
      (modifyTileCount count tile $ rack state)

solveRummikubState :: RummikubState -> IO (Maybe ([Set], [Tile]))
solveRummikubState state =
  let
    sArr = initSParameters allSets
    model = initModel sArr (table state) (rack state)
  in
    solveModel model
