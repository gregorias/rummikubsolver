{-# LANGUAGE OverloadedLists #-}

module Interface.TileChangeCommand (
  TileChangeCommand (..),
  parseTileChangeCommand,
) where

import Closed (closed)
import Data.List (head, partition, tail)
import Data.List.NonEmpty (some1)
import Data.Text qualified as Text
import Game.Core (
  Color (..),
  Tile (..),
  Value,
 )
import Relude hiding (head, tail)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP

data TileChangeCommand = TileChangeCommand
  { tccRemove :: ![Tile]
  , tccAdd :: ![Tile]
  }
  deriving stock (Eq, Show)

type Parser = MP.Parsec Void Text

-- | Parse a list of tile specifications from input.
parseTileChangeCommand :: Text -> TileChangeCommand
parseTileChangeCommand input =
  if any isNothing tiles
    then TileChangeCommand [] []
    else TileChangeCommand (concatMap toList $ catMaybes removeTiles) (concatMap toList $ catMaybes addTiles)
 where
  tileStringToTile :: Text -> Maybe (NonEmpty Tile)
  tileStrings =
    map (Text.unpack . Text.strip)
      . Text.splitOn (Text.pack ",")
      $ input
  (removeStrings, addStrings) =
    partition
      ((&&) <$> not . null <*> (== '-') . head)
      tileStrings
  addTiles = map (tileStringToTile . fromString) addStrings
  removeTiles = map (tileStringToTile . fromString . tail) removeStrings
  tiles = removeTiles ++ addTiles
  tileStringToTile = MP.parseMaybe tilesP

tilesP :: Parser (NonEmpty Tile)
tilesP = MP.label "tile spec" $ (one <$> jokerP) <|> nonJokersP

colorP :: Parser Color
colorP = MP.label "color" $ asum [MP.string c >> return color | (c, color) <- colorMap]
 where
  colorMap =
    [ ("r", Red)
    , ("l", Blue)
    , ("y", Yellow)
    , ("b", Black)
    ]

colorsP :: Parser (NonEmpty Color)
colorsP = MP.label "color spec" $ some1 colorP

valueP :: Parser Value
valueP = MP.label "value" $ do
  val :: Integer <- MP.decimal
  maybe
    (fail $ "expected a value in the range of 1-13, but got " <> show val)
    return
    (closed val)

valuesP :: Parser (NonEmpty Value)
valuesP = MP.label "value spec" $ do
  beg <- valueP
  endMaybe <- optional (MP.char '-' >> valueP)
  maybe
    (return $ one beg)
    (\end -> return [beg .. end])
    endMaybe

jokerP :: Parser Tile
jokerP = MP.label "joker" $ MP.string "j" >> return Joker

nonJokersP :: Parser (NonEmpty Tile)
nonJokersP = MP.label "non-joker tile spec" $ do
  colors <- colorsP
  values <- valuesP
  return $ do
    color <- colors
    value <- values
    return $ ValueTile (value, color)
