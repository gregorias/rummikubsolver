module Interface.Common (parseTiles) where

import Data.Either
import Data.List (partition)
import Data.Maybe
import Data.Text qualified as Text
import Game (
    Color (..),
    Tile (..),
    maxValue,
    minValue,
 )
import Safe qualified

-- | Parse a list of tile specifications from input.
parseTiles :: String -> ([Game.Tile], [Game.Tile])
parseTiles input =
    if not (null (lefts tiles))
        then ([], [])
        else (concat $ rights removeTiles, concat $ rights addTiles)
  where
    tileStringToTile :: String -> Either String [Game.Tile]
    tileStringToTile tileString
        | null tileString = Left "Provided empty string. Empty string is invalid."
        | tileString == "j" = Right [Game.Joker]
        | null colors
            || null values
            || head values
            < Game.minValue
            || last values
            > Game.maxValue =
            Left $ "Could not parse string: " ++ tileString
        | otherwise = Right $ [Game.ValueTile (v, c) | v <- values, c <- colors]
      where
        colorMap =
            [ ('r', Game.Red)
            , ('l', Game.Blue)
            , ('y', Game.Yellow)
            , ('b', Game.Black)
            ]
        colorChars = map fst colorMap
        (colorsPrefix, valueSuffix) =
            span (`elem` colorChars) tileString
        colors :: [Game.Color]
        colors = mapMaybe (`lookup` colorMap) colorsPrefix
        values = parseValueSuffix valueSuffix :: [Int]
        parseValueSuffix :: String -> [Int]
        parseValueSuffix valueStr =
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
