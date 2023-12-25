-- | Data and functions for solving a Rummikub game.
module Solver (
  solveTable,
  solveTableAndRack,
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
  tileCount,
 )
import Game.TileCountArray qualified as Tca
import Relude hiding (Set)
import Relude.Unsafe qualified as Unsafe
import System.IO.Unsafe (unsafePerformIO)

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

-- | Solves a table.
--
-- If the table is not solvable, returns Nothing.
solveTable ::
  -- | Tiles on a table.
  TileCountArray ->
  Maybe [Set]
solveTable table = do
  let sArr = initSParameters allSets
  let model = initModel sArr Tca.empty table
  (sets, tilesToPlace) <- unsafePerformIO $ solveModel model
  guard $ sort tilesToPlace == sort (Tca.toElemList table)
  return sets

-- | Solves the game given a table and a rack.
solveTableAndRack ::
  -- | Tiles on a table.
  TileCountArray ->
  -- | Tiles on a rack.
  TileCountArray ->
  IO (Maybe ([Set], [Tile]))
solveTableAndRack table rack = do
  let sArr = initSParameters allSets
  let model = initModel sArr table rack
  solveModel model
