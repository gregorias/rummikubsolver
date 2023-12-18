{-
Module : Game
Description : Data and functions for describing and solving a Rummikub game.
-}
module Game (
  RummikubState,
  table,
  rack,
  initialRummikubState,
  modifyTableMay,
  modifyRackMay,
  solveRummikubState,
) where

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
import Data.Map.Lazy (union)
import Data.Map.Lazy qualified as Data.Map
import Game.Core (
  Tile (..),
 )
import Game.Set (Set, allSets)
import Game.Set qualified as Set
import Game.TileCountArray (
  TileCountArray,
  addCount,
  tileCount,
  toRawArray,
 )
import Game.TileCountArray qualified as TileCountArray
import Relude hiding (Set)
import Relude.Unsafe qualified as Unsafe

initSParameters :: [Set] -> UArray (Int, Int) Int
initSParameters sets =
  accumArray (\count _ -> count + 1) 0 sArrayBounds assocList
 where
  setAndTiles :: [(Set, [Tile])]
  setAndTiles = map (\set -> (set, Set.toTiles set)) sets
  arrayIndexes :: [(Int, Int)]
  arrayIndexes =
    concatMap
      (\(set, setTiles) -> map (fromEnum set,) (fromEnum <$> setTiles))
      setAndTiles
  assocList = map (,error "initSParameters") arrayIndexes
  setCount = length sets
  sArrayBounds = ((0, 0), (setCount, fromEnum (maxBound :: Tile)))

initModel ::
  (IArray a Int, MonadState (Data.LinearProgram.LP (Int, Int) Int) m) =>
  a (Int, Int) Int ->
  TileCountArray ->
  TileCountArray ->
  m ()
initModel sArr table rack = do
  setDirection Max
  setObjective $ Relude.fromList $ map (curry (,1) 0) [0 .. tileSize]
  variableKinds setSize
  setVariableBounds setSize
  rackVariableBounds rack
  tileConstraints sArr table
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
  (MonadState (Data.LinearProgram.LP (Int, Int) Int) m) =>
  TileCountArray ->
  m ()
rackVariableBounds rackArg =
  mapM_ (\t -> varLeq (0, fromEnum t) (tileCount t rackArg)) [minBound .. maxBound]

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
  TileCountArray ->
  m ()
tileConstraints sArr table =
  mapM_ createTileConstraint [minBound .. maxBound]
 where
  setSize = fst . snd . bounds $ sArr
  createTileConstraint tile =
    equalTo
      (tileUsedInSetsCombination `union` tilesPlacedFromRack)
      (tileCount tile table)
   where
    tileUsedInSetsCombination :: Map (Int, Int) Int
    tileUsedInSetsCombination =
      Relude.fromList
        $ map
          (\setIndex -> ((1, setIndex), sArr ! (setIndex, fromEnum tile)))
          [0 .. setSize]
    tilesPlacedFromRack :: Map (Int, Int) Int
    tilesPlacedFromRack = Relude.fromList [((0, fromEnum tile), -1)]

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
  setIdxToSet = (Unsafe.!!) allSets

--- Game State
data RummikubState = RummikubState
  { table :: TileCountArray
  , rack :: TileCountArray
  }
  deriving stock (Eq, Show)

initialRummikubState :: RummikubState
initialRummikubState =
  RummikubState TileCountArray.empty TileCountArray.empty

isTileArrayConsistent :: UArray Int Int -> Bool
isTileArrayConsistent = all (<= 2) . elems

isRummikubStateConsistent :: RummikubState -> Bool
isRummikubStateConsistent state =
  isTileArrayConsistent tableArray
    && isTileArrayConsistent rackArray
    && isTileArrayConsistent combinedArrays
 where
  tableArray = toRawArray state.table
  rackArray = toRawArray state.rack
  combinedArrays = accum (+) tableArray (assocs rackArray)

modifyTableMay :: Int -> Tile -> RummikubState -> Maybe RummikubState
modifyTableMay count tile state = do
  newTable <- addCount count tile state.table
  let newState = RummikubState newTable state.rack
  guard $ isRummikubStateConsistent newState
  return newState

modifyRackMay :: Int -> Tile -> RummikubState -> Maybe RummikubState
modifyRackMay count tile state = do
  newRack <- addCount count tile state.rack
  let newState = RummikubState state.table newRack
  guard $ isRummikubStateConsistent newState
  return newState

solveRummikubState :: RummikubState -> IO (Maybe ([Set], [Tile]))
solveRummikubState state =
  let sArr = initSParameters allSets
      model = initModel sArr state.table state.rack
   in solveModel model
