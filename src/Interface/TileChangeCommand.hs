{-# LANGUAGE OverloadedLists #-}

module Interface.TileChangeCommand (
  TileChangeCommand (..),
  parseTileChangeCommand,
) where

import Closed (closed)
import Data.List.Extra (PartitionSelect (..), partition)
import Data.List.NonEmpty (some1)
import Game.Core (
  Color (..),
  Tile (..),
  Value,
 )
import Relude hiding (head, tail)
import Text.Megaparsec (sepBy)
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
  let (tcc'sMaybe :: Maybe [TileChangeCommand']) = MP.parseMaybe tileChangeCommandsP input
   in maybe
        (TileChangeCommand [] [])
        ( \tcc's ->
            let (addTiles, removeTiles) =
                  partition (\case Add ts -> L ts; Remove ts -> R ts) tcc's
             in TileChangeCommand (concat removeTiles) (concat addTiles)
        )
        tcc'sMaybe

data TileChangeCommand' = Add ![Tile] | Remove ![Tile]

tileChangeCommandsP :: Parser [TileChangeCommand']
tileChangeCommandsP = MP.label "tile change commands" $ do
  sepBy tileChangeCommandP (MP.string ",")

tileChangeCommandP :: Parser TileChangeCommand'
tileChangeCommandP = MP.label "tile change command" $ do
  (op :: [Tile] -> TileChangeCommand') <- maybe Add (const Remove) <$> MP.optional (MP.string "-")
  (tiles :: NonEmpty Tile) <- tilesP
  return $ op (toList tiles)

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
