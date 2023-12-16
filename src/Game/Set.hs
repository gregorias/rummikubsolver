module Game.Set (
  Set,
  toTiles,
  allSets,
) where

import Combinatorics
import Data.HashMap.Strict qualified as HashMap
import Game.Core (
  Color,
  Tile (..),
  allColors,
  allValues,
  maxSingleTileCount,
  maxValue,
  minSingleTileCount,
  minValue,
 )
import Relude hiding (Set)
import Relude.Unsafe qualified as Unsafe

-- | A valid set of tiles.
--
-- Invariant: the set is naturally sorted.
newtype Set = Set [Tile]
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

instance Bounded Set where
  minBound = Unsafe.head allSets
  maxBound = Unsafe.last allSets

instance Enum Set where
  fromEnum set =
    fromIntegral
      $ HashMap.lookupDefault
        (error "Set.fromEnum: bad argument")
        set
        allSetsAsHashMap
  toEnum n = allSets Unsafe.!! n

toTiles :: Set -> [Tile]
toTiles (Set set) = set

-- | All possible sets.
allSets :: [Set]
allSets = seqSets ++ colorSets
 where
  seqSets =
    concat
      [ generateAllSeqSets c j s
      | c <- allColors
      , j <- [minSingleTileCount .. maxSingleTileCount]
      , s <- [3 .. 5]
      ]
  colorSets =
    concat
      $ [generateAllColorSets j 4 | j <- [minSingleTileCount .. maxSingleTileCount]]
      ++ [generateAllColorSets j 3 | j <- [minSingleTileCount .. (maxSingleTileCount - 1)]]

-- Keeping the index to help with Enum.
allSetsAsHashMap :: HashMap Set Natural
allSetsAsHashMap = HashMap.fromList $ zip allSets [0 ..]

generateAllSeqSets :: Color -> Natural -> Int -> [Set]
generateAllSeqSets color jokerCount setSize =
  concatMap
    generateSetsFrom
    [minValue .. (maxValue - setSize + fromIntegral jokerCount + 1)]
 where
  valuesToTiles = map (\value -> ValueTile (value, color))
  generateSetsFrom :: Int -> [Set]
  generateSetsFrom begVal =
    map
      ( Set
          . ((++) (replicate (fromIntegral jokerCount) Joker) . valuesToTiles)
          . (fromIntegral begVal :)
      )
      ( generateCombinations
          [fromIntegral begVal + 1 .. fromIntegral maxIntervalValue]
          (fromIntegral setSize - jokerCount - 1)
      )
   where
    maxIntervalValue = min maxValue $ begVal + setSize - 1

generateAllColorSets :: Natural -> Int -> [Set]
generateAllColorSets jokerCount setSize = do
  colorComb <- colorCombs
  value <- allValues
  let (set :: [Tile]) = jokers ++ map (\c -> ValueTile (value, c)) colorComb
  return $ Set set
 where
  colorCombs = generateCombinations allColors $ fromIntegral setSize - jokerCount
  jokers :: [Tile]
  jokers = replicate (fromIntegral jokerCount) Joker
