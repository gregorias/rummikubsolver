module Interface.Console (
  game,
) where

import Data.Array.IArray qualified as Array
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (foldrM)
import Game (
  solveRummikubState,
 )
import Game.Core (
  Color (..),
  Tile (..),
 )
import Game.Set qualified as Set
import Game.State (
  RummikubState,
  initialRummikubState,
  modifyRack,
  modifyTable,
  rack,
  table,
 )
import Game.TileCountArray (TileCountArray)
import Game.TileCountArray qualified as TileCountArray
import Interface.TileChangeCommand (
  TileChangeCommand (..),
  parseTileChangeCommands,
 )
import Relude
import Relude.Unsafe qualified as Unsafe
import Safe qualified

type Game = StateT RummikubState IO ()

data MenuCommand = MenuCommand
  { commandSymbol :: Char
  , commandAction :: Game
  }

commands :: [MenuCommand]
commands = zipWith MenuCommand ['1' ..] actions'
 where
  actions :: [Game]
  actions =
    [ modifyCommand (\cs -> eitherToMaybe . modifyTable cs)
    , modifyCommand (\cs -> eitherToMaybe . modifyRackBatch cs)
    , solveCommand
    , showCommand
    , restartCommand
    ]
  actions' = (++ [return ()]) . map (>> game) $ actions
  modifyRackBatch :: [(Int, Tile)] -> RummikubState -> Either Text RummikubState
  modifyRackBatch changes state = foldrM (uncurry modifyRack) state changes

putCommandPrompt :: IO ()
putCommandPrompt = do
  putStrLn "Please provide a command: "
  putStrLn "1. Modify table."
  putStrLn "2. Modify rack."
  putStrLn "3. Solve current state."
  putStrLn "4. Show current state."
  putStrLn "5. Restart."
  putStrLn "6. Quit."

modifyCommand :: ([(Int, Tile)] -> RummikubState -> Maybe RummikubState) -> Game
modifyCommand modifyFunction = do
  tileChangeCommands <- liftIO readTiles
  currentState <- get
  let (changes :: [(Int, Tile)]) = flip map tileChangeCommands $ \case
        Add tile -> (1, tile)
        Remove tile -> (-1, tile)
  let newStateMay = modifyFunction changes currentState
  maybe
    (liftIO $ putStrLn "This modification would lead to invalid state.")
    put
    newStateMay
  liftIO $ putStrLn ""

solveCommand :: Game
solveCommand = do
  solveAction <- solveRummikubState <$> get
  liftIO $ do
    maybeSetsAndTiles <- solveAction
    case maybeSetsAndTiles of
      Nothing -> putStrLn "The linear solver could not solve current state."
      Just (ss, ts) -> do
        putStrLn ("Found sets:\n" ++ prettyPrintSets (Set.toTiles <$> ss))
        putStrLn ("Tiles placed from rack: " ++ prettyPrintTiles ts)
        putStrLn ""
 where
  prettyPrintTiles =
    separateWithAComma . map prettyPrint
  prettyPrintSets =
    concatMap (("    " ++) . (++ "\n") . prettyPrintTiles)

showCommand :: Game
showCommand = do
  curState <- get
  liftIO . putStrLn . (++ "\n") . prettyPrint $ curState

restartCommand :: Game
restartCommand = put initialRummikubState

-- | The main repl of the console interface.
game :: Game
game = do
  liftIO putCommandPrompt
  menuOption <- Unsafe.head . toString <$> liftIO getLine
  liftIO $ putStrLn ""
  let action =
        Safe.headDef game
          . map commandAction
          . filter ((==) menuOption . commandSymbol)
          $ commands
  action

-- | Read a list of tile specifications from input.
readTiles :: IO [TileChangeCommand]
readTiles = do
  putStrLn
    ( "Provide list of tiles in a form TILE [, TILE]*, \n"
        ++ "where TILE ::= [-] COLOR VALUE, COLOR ::= [rlyb]+, "
        ++ "VALUE ::= INT | INT - INT (write '-' to remove the tile): "
    )
  commandText <- getLine
  parseTileChangeCommands commandText
    & either
      (\error -> putStrLn (toString error) >> return [])
      return

separateWithAComma :: [String] -> String
separateWithAComma (x : y : xs) = x ++ ", " ++ separateWithAComma (y : xs)
separateWithAComma [x] = x
separateWithAComma [] = ""

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Tile where
  prettyPrint Joker = "Joker"
  prettyPrint (ValueTile (i, c)) = show c ++ "[" ++ show (fromIntegral i) ++ "]"

instance PrettyPrint TileCountArray where
  prettyPrint arr =
    "RED: "
      ++ tileListToString reds
      ++ "\n"
      ++ "BLUE: "
      ++ tileListToString blues
      ++ "\n"
      ++ "YELLOW: "
      ++ tileListToString yellows
      ++ "\n"
      ++ "BLACK: "
      ++ tileListToString blacks
      ++ "\n"
      ++ "JOKERS: "
      ++ tileListToString joker
   where
    elements =
      filter ((> 0) . snd)
        . map (\(i, e) -> (toEnum i :: Tile, e))
        . Array.assocs
        . TileCountArray.toRawArray
        $ arr
    matchColor :: Color -> Tile -> Bool
    matchColor _ Joker = False
    matchColor c (ValueTile (_, c')) = c == c'
    reds = filter (matchColor Red . fst) elements
    blues = filter (matchColor Blue . fst) elements
    yellows = filter (matchColor Yellow . fst) elements
    blacks = filter (matchColor Black . fst) elements
    joker = filter ((== Joker) . fst) elements
    tileListToString :: [(Tile, Int)] -> String
    tileListToString =
      separateWithAComma
        . concatMap (\(t, i) -> replicate i (prettyPrint t))

instance PrettyPrint RummikubState where
  prettyPrint curState =
    "-----\n"
      ++ "TABLE\n"
      ++ prettyPrint (table curState)
      ++ "\n"
      ++ "-----\n"
      ++ "RACK\n"
      ++ prettyPrint (rack curState)
