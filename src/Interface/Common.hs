module Interface.Common (parseTiles) where

import Closed.Extra (closedEither)
import Control.Monad (when)
import Data.Either (lefts, rights)
import Data.Either.Extra (mapLeft)
import Data.List (partition)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Game.Core (
  Color (..),
  Tile (..),
  Value,
 )
import Safe qualified

-- | Parse a list of tile specifications from input.
parseTiles :: String -> ([Tile], [Tile])
parseTiles input =
  if not (null (lefts tiles))
    then ([], [])
    else (concat $ rights removeTiles, concat $ rights addTiles)
 where
  tileStringToTile :: String -> Either String [Tile]
  tileStringToTile tileString
    | null tileString = Left "Provided empty string. Empty string is invalid."
    | tileString == "j" = Right [Joker]
    | null colors = Left $ "Could not parse string: " ++ tileString
    | otherwise = do
        values <- mapLeft unpack valuesEither
        when (null values) $ Left ("Could not parse string: " ++ tileString)
        Right $ [ValueTile (v, c) | v <- values, c <- colors]
   where
    colorMap =
      [ ('r', Red)
      , ('l', Blue)
      , ('y', Yellow)
      , ('b', Black)
      ]
    colorChars = map fst colorMap
    (colorsPrefix, valueSuffix) =
      span (`elem` colorChars) tileString
    colors :: [Color]
    colors = mapMaybe (`lookup` colorMap) colorsPrefix
    valuesEither :: Either Text [Value]
    valuesEither = mapM (closedEither @1 @13 . fromIntegral) (parseIntSuffix valueSuffix)

    -- \| Parses a string of the form "1-3" or "5" into a list of integers.
    parseIntSuffix :: String -> [Int]
    parseIntSuffix valueStr =
      let bounds :: [Maybe Int]
          bounds =
            map (Safe.readMay . Text.unpack . Text.strip)
              . Text.splitOn (Text.pack "-")
              . Text.pack
              $ valueStr
       in if length bounds
            > 2
            || null bounds
            || any isNothing bounds
            then []
            else
              if length bounds == 2
                then [(fromJust $ head bounds) .. (fromJust $ bounds !! 1)]
                else [fromJust $ head bounds]
  tileStrings =
    map (Text.unpack . Text.strip)
      . Text.splitOn (Text.pack ",")
      . Text.pack
      $ input
  (removeStrings, addStrings) =
    partition
      ((&&) <$> not . null <*> (== '-') . head)
      tileStrings
  addTiles = map tileStringToTile addStrings
  removeTiles = map (tileStringToTile . tail) removeStrings
  tiles = removeTiles ++ addTiles
