module Interface.Common where

import Control.Applicative
import Data.Either
import Data.List (partition)
import Data.Maybe
import qualified Data.Text as Text
import Game (
  Color(..)
  , Tile(..)
  , maxValue
  , minValue
  )
import qualified Safe

-- | Parse a list of tile specifications from input.
parseTiles :: String -> ([Game.Tile], [Game.Tile])
parseTiles input = do
  if length (lefts tiles) > 0
  then ([], [])
  else (concat $ rights removeTiles, concat $ rights addTiles)
  where
    tileStringToTile :: String -> Either String [Game.Tile]
    tileStringToTile tileString =
      if length tileString == 0
      then Left "Provided empty string. Empty string is invalid."
      else 
        if tileString == "j"
        then Right [Game.Joker]
        else
          if length colors == 0 || length values == 0 
            || head values < Game.minValue || last values > Game.maxValue
          then Left $ "Could not parse string: " ++ tileString
          else Right $ [Game.ValueTile (v, c) | v <- values, c <- colors]
      where
        colorMap = [('r', Game.Red)
          , ('l', Game.Blue)
          , ('y', Game.Yellow)
          , ('b', Game.Black)]
        colorChars = map fst colorMap
        (colorsPrefix, valueSuffix) = break 
          (not . flip elem colorChars)
          tileString
        colors :: [Game.Color]
        colors = catMaybes $ map (flip lookup colorMap) colorsPrefix
        values = parseValueSuffix valueSuffix :: [Int]
        parseValueSuffix :: String -> [Int]
        parseValueSuffix valueStr = 
          let 
            bounds :: [Maybe Int]
            bounds = map (Safe.readMay . Text.unpack . Text.strip)
              . Text.splitOn (Text.pack "-")
              . Text.pack 
              $ valueStr
          in
            if length bounds > 2 || length bounds == 0 
                || length (filter isNothing bounds) > 0
            then []
            else
              if length bounds == 2
              then [(fromJust $ bounds !! 0) .. (fromJust $ bounds !! 1)]
              else [(fromJust $ bounds !! 0)]
    tileStrings = map (Text.unpack . Text.strip) . Text.splitOn (Text.pack ",")
      . Text.pack
      $ input
    (removeStrings, addStrings) = partition
      ((&&) <$> ((> 0) . length) <*> ((== '-') . head))
      tileStrings
    addTiles = map tileStringToTile addStrings
    removeTiles = map (tileStringToTile . tail) removeStrings
    tiles = removeTiles ++ addTiles
