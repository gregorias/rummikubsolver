{-
Module : Game
Description : Data and functions for describing and solving a Rummikub game.
-}
module Game (
  TileArray,
  RummikubState,
  table,
  rack,
  initialRummikubState,
  tileArrayElems,
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
import Relude hiding (Set)
import Relude.Unsafe qualified as Unsafe

initSParameters :: (IArray a Int) => [Set] -> a (Int, Int) Int
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
  a Int Int ->
  a Int Int ->
  m ()
initModel sArr tableArg rackArg = do
  setDirection Max
  setObjective $ Relude.fromList $ map (curry (,1) 0) [0 .. tileSize]
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
      Relude.fromList
        $ map
          (\setIndex -> ((1, setIndex), sArr ! (setIndex, tileIndex)))
          [0 .. setSize]
    tilesPlacedFromRack :: Map (Int, Int) Int
    tilesPlacedFromRack = Relude.fromList [((0, tileIndex), -1)]

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
