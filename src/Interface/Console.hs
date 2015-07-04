module Interface.Console (
    game
    ) where

import Control.Applicative
import qualified Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Array.IArray as Array
import Data.Either
import Data.Maybe
import Data.List (partition)
import qualified Data.Text as Text
import qualified Data.Map.Lazy as Map
import Game (
  Color(..)
  , RummikubState
  , Set
  , Tile(..)
  , TileArray
  , initialRummikubState
  , maxValue
  , minValue
  , modifyRackMay
  , modifyTableMay
  , rack
  , solveRummikubState
  , table
  )
import qualified Safe

type Game = StateT RummikubState IO ()

data MenuCommand = MenuCommand {
  commandSymbol :: Char
  , commandAction :: Game
  }

commands :: [MenuCommand]
commands = map (uncurry MenuCommand) (zip ['1' ..] actions')
  where
    actions :: [Game]
    actions = [
      modifyCommand modifyTableMay
      , modifyCommand modifyRackMay
      , solveCommand
      , showCommand
      , restartCommand
      ]
    actions' = (++ [return ()]) . map (>> game) $ actions

putCommandPrompt :: IO ()
putCommandPrompt = do
  putStrLn "Please provide a command: "
  putStrLn "1. Modify table."
  putStrLn "2. Modify rack."
  putStrLn "3. Solve current state."
  putStrLn "4. Show current state."
  putStrLn "5. Restart."
  putStrLn "6. Quit."

modifyCommand :: (Int -> Tile -> RummikubState -> Maybe RummikubState) -> Game
modifyCommand modifyFunction = do
  (removedTiles, newTiles) <- liftIO readTiles
  newStateMay <- fmap (addTiles 1 newTiles <=< addTiles (-1) removedTiles) $ get
  maybe 
    (liftIO $ putStrLn "This modification would lead to invalid state.")
    put
    newStateMay
  liftIO $ putStrLn ""
  where
    addTiles :: Int -> [Tile] -> RummikubState -> Maybe RummikubState
    addTiles count tiles = foldr (>=>) (Just . id)
      (map (modifyFunction count) tiles)

solveCommand :: Game
solveCommand = do
  solveAction <- fmap solveRummikubState $ get
  liftIO $ do 
    maybeSetsAndTiles <- solveAction
    case maybeSetsAndTiles of
      Nothing -> putStrLn "The linear solver could not solve current state."
      Just (ss, ts) -> do
        putStr $ "Found sets:\n" ++ prettyPrintSets ss ++ "\n"
        putStr $ "Tiles placed from rack: " ++ prettyPrintTiles ts ++ "\n"
        putStrLn ""
  where
    prettyPrintTiles =
      separateWithAComma . map prettyPrint
    prettyPrintSets = 
      concatMap (("    " ++) . (++ "\n") . prettyPrintTiles)

showCommand :: Game
showCommand = do
  state <- get
  liftIO . putStrLn . (++ "\n") . prettyPrint $ state

restartCommand :: Game
restartCommand = put initialRummikubState

-- | The main repl of the console interface.
game :: Game
game = do
  liftIO putCommandPrompt
  menuOption <- head <$> liftIO getLine
  liftIO $ putStrLn ""
  let 
    action = Safe.headDef game 
      . map commandAction 
      . filter ((==) menuOption . commandSymbol) 
      $ commands
  action

-- | Read a list of tile specifications from input.
readTiles :: IO ([Tile], [Tile])
readTiles = do
  putStrLn ("Provide list of tiles in a form TILE [, TILE]*, \n" ++
    "where TILE ::= [-] COLOR VALUE, COLOR ::= [rlyb]+, " ++ 
    "VALUE ::= INT | INT - INT (write '-' to remove the tile): ")
  listOfTiles <- getLine
  let
    tileStrings = map (Text.unpack . Text.strip) . Text.splitOn (Text.pack ",")
      . Text.pack
      $ listOfTiles
    (removeStrings, addStrings) = partition
      ((&&) <$> ((> 0) . length) <*> ((== '-') . head))
      tileStrings
    addTiles = map tileStringToTile addStrings
    removeTiles = map (tileStringToTile . tail) removeStrings
    tiles = removeTiles ++ addTiles
  sequence_ . map putStrLn . lefts $ tiles
  if length (lefts tiles) > 0
  then return ([], [])
  else return $ (concat $ rights removeTiles, concat $ rights addTiles)
  where
    tileStringToTile :: String -> Either String [Tile]
    tileStringToTile tileString =
      if length tileString == 0
      then Left "Provided empty string. Empty string is invalid."
      else 
        if tileString == "j"
        then Right [Joker]
        else
          if length colors == 0 || length values == 0 
            || head values < minValue || last values > maxValue
          then Left $ "Could not parse string: " ++ tileString
          else Right $ [ValueTile (v, c) | v <- values, c <- colors]
      where
        colorMap = [('r', Red), ('l', Blue), ('y', Yellow), ('b', Black)]
        colorChars = map fst colorMap
        (colorsPrefix, valueSuffix) = break 
          (not . flip elem colorChars)
          tileString
        colors :: [Color]
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
              
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\_ -> Nothing) Just

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

separateWithAComma :: [String] -> String
separateWithAComma (x : y : xs) = x ++ ", " ++ separateWithAComma (y : xs)
separateWithAComma (x : []) = x
separateWithAComma [] = ""

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Tile where
  prettyPrint Joker = "Joker"
  prettyPrint (ValueTile (i, c)) = show c ++ "[" ++ show i ++ "]"

instance PrettyPrint TileArray where
  prettyPrint arr = 
    "RED: " ++ (tileListToString reds) ++ "\n" ++
    "BLUE: " ++ (tileListToString blues) ++ "\n" ++
    "YELLOW: " ++ (tileListToString yellows) ++ "\n" ++
    "BLACK: " ++ (tileListToString blacks) ++ "\n" ++
    "JOKERS: " ++ (tileListToString joker)
    where
      elements = filter ((> 0) . snd) 
        . map (\(i, e) -> (toEnum i :: Tile, e)) . Array.assocs 
        $ arr
      matchColor :: Color -> Tile -> Bool
      matchColor c Joker = False
      matchColor c (ValueTile (_, c')) = c == c'
      reds = filter (matchColor Red . fst) elements
      blues = filter (matchColor Blue . fst) elements
      yellows = filter (matchColor Yellow . fst) elements
      blacks = filter (matchColor Black . fst) elements
      joker = filter ((== Joker) . fst) elements
      tileListToString :: [(Tile, Int)] -> String
      tileListToString =  separateWithAComma 
        . concatMap (\(t, i) -> replicate i (prettyPrint t))

instance PrettyPrint RummikubState where
  prettyPrint state =
    "-----\n" ++ 
    "TABLE\n" ++ 
    prettyPrint (table state) ++ "\n" ++
    "-----\n" ++ 
    "RACK\n" ++ 
    prettyPrint (rack state)
