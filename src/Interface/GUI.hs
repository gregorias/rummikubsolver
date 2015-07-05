module Interface.GUI (
    game
    , GUIConfig
    , guiPort
    , guiCssPath
    , defaultGUIConfig
    ) where

import Control.Monad
import qualified Data.Array.IArray as Array
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Reactive.Threepenny as FRP
import qualified Safe

import qualified Game as Game

data GUIConfig = GUIConfig {
  guiPort :: Int
  , guiCssPath :: String
  }

defaultGUIConfig :: GUIConfig
defaultGUIConfig = GUIConfig {
  guiPort = 8080
  , guiCssPath = "resources"
}

game :: GUIConfig -> IO ()
game config = do
  startGUI 
    defaultConfig {
      jsPort = Just $ guiPort config
      , jsStatic = Just $ guiCssPath config
      , jsLog = \_ -> return ()
      }
    setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "RummikubSolver"
  UI.addStyleSheet window "main.css"

  title <- UI.h1 # set text "RummikubSolver"

  let state = Game.initialRummikubState
  (stateChangeEvent, stateChangeHandler) <- liftIO FRP.newEvent
  stateEvent <- FRP.accumE state stateChangeEvent
  stateBehavior <- FRP.stepper Game.initialRummikubState stateEvent
  let
    tableBehavior = fmap (Game.tileArrayElems . Game.table) stateBehavior
    rackBehavior = fmap (Game.tileArrayElems . Game.rack) stateBehavior

  (solutionEvent, solutionHandler) <- liftIO FRP.newEvent
  liftIO $ FRP.register 
    stateEvent 
    (\state -> do
      maybeSolution <- Game.solveRummikubState state
      maybe (solutionHandler ([], [])) solutionHandler maybeSolution
      )
  solutionBehavior <- FRP.stepper ([], []) solutionEvent
  let
    solutionSetsBehavior = fmap fst solutionBehavior
    solutionTilesBehavior = fmap snd solutionBehavior
      
  tableTileTable <- tileTable tableBehavior
  element tableTileTable # set UI.id_ "tableGrid"

  rackTileTable <- tileTable rackBehavior
  element rackTileTable # set UI.id_ "rackGrid"

  tableCommandBar <- commandRow 
    "Change table: "
    Game.modifyTableMay
    stateChangeHandler

  rackCommandBar <- commandRow 
    "Change rack: "
    Game.modifyRackMay
    stateChangeHandler
  tableTileTableWrap <- UI.div #. "tileTableWrap"
    #+ [element tableTileTable, element tableCommandBar]
  rackTileTableWrap <- UI.div #. "tileTableWrap"
    #+ [element rackTileTable, element rackCommandBar]

  setsBox <- setsDiv solutionSetsBehavior
  placedTilesBox <- placedTilesDiv solutionTilesBehavior

  getBody window #+ [
    element title
    , element tableTileTableWrap
    , element rackTileTableWrap
    , element setsBox
    , element placedTilesBox
    ]
  return ()
  where
    safeModify :: 
      (Int -> Game.Tile -> Game.RummikubState -> Maybe Game.RummikubState)
      -> Int
      -> Game.Tile
      -> Game.RummikubState
      -> Game.RummikubState
    safeModify modifyFunctionMay count tile state =  
      maybe state id $ modifyFunctionMay count tile state

-- | Configure a command row
commandRow :: String
  -> (Int -> Game.Tile -> Game.RummikubState -> Maybe Game.RummikubState)
  -> FRP.Handler (Game.RummikubState -> Game.RummikubState)
  -> UI Element
commandRow prompt modifyFunction stateChangeHandler = do
  promptElement <- string prompt
  inputBox <- UI.input
  button <- UI.button # set UI.text "Modify" :: UI Element

  on UI.click button 
    $ \_ -> 
      let
        modifyTiles :: Int
          -> [Game.Tile] 
          -> Game.RummikubState 
          -> Maybe Game.RummikubState
        modifyTiles count tiles = foldr (>=>) (Just . id)
          $ map (modifyFunction count) tiles
        modifyMaybeSafe :: (s -> Maybe s) -> s -> s
        modifyMaybeSafe modifyFunction state =
          maybe state id $ modifyFunction state
      in do
      inputString <- inputBox # get value :: UI String
      let 
        (removedTiles, newTiles) = parseTiles inputString
        modifyFunctionSafe = modifyMaybeSafe 
          (modifyTiles 1 newTiles <=< modifyTiles (-1) removedTiles) 
      liftIO $ stateChangeHandler modifyFunctionSafe
  UI.div #+ [element promptElement, element inputBox, element button]

  
-- | Configure the tile table display.
tileTable :: FRP.Behavior [Game.Tile] -> UI Element
tileTable tileListBehavior = do
  mainBox <- UI.div #. "tileTable" :: UI Element
  let
    childrenBehavior :: FRP.Behavior (UI [Element])
    childrenBehavior = fmap tileGrid tileListBehavior
  onChanges childrenBehavior 
    (\newChildrenUI -> do
      newChildren <- newChildrenUI
      set children newChildren $ element mainBox
    )
  --mainBox <- sink children childrenBehavior $ element mainBox
  return mainBox
  where
    tileGrid :: [Game.Tile] -> UI [Element]
    tileGrid tiles = do
      tableGrid <- grid . map (map tile) $ groupList 13 tiles :: UI Element
      return [tableGrid]

groupList :: Int -> [Game.Tile] -> [[Game.Tile]]
groupList _ [] = []
groupList size ss = take size ss : groupList size (drop size ss)

tile :: Game.Tile -> UI Element
tile tileArg = do
  tileDiv <- UI.div #. ("tile " ++ tileColor tileArg)
    # set text (tileText tileArg)
  return tileDiv
  where
    tileText :: Game.Tile -> String
    tileText Game.Joker = "J"
    tileText (Game.ValueTile (v, _)) = show v

    tileColor :: Game.Tile -> String
    tileColor Game.Joker = "black"
    tileColor (Game.ValueTile (_, Game.Red)) = "red"
    tileColor (Game.ValueTile (_, Game.Blue)) = "blue"
    tileColor (Game.ValueTile (_, Game.Yellow)) = "yellow"
    tileColor (Game.ValueTile (_, Game.Black)) = "black"

setsDiv :: FRP.Behavior [[Game.Tile]] -> UI Element
setsDiv setsBehavior = do
  setsString <- UI.div #+ [string "Possible sets: "]
  setsBox <- UI.div #. "sets"
  let setDivBehavior = fmap (map setDiv) setsBehavior
  onChanges setDivBehavior 
    (\newChildrenUI -> do
      newChildren <- sequence newChildrenUI
      set children newChildren $ element setsBox
    )
  UI.div #. "setsWrap"
    #+ [element setsString, element setsBox, UI.div #. "clear"]
  where
    setDiv :: [Game.Tile] -> UI Element
    setDiv ss = do
      row (map tile ss) #. "set"

placedTilesDiv :: FRP.Behavior [Game.Tile] -> UI Element
placedTilesDiv placedTilesBehavior = do
  descriptionString <- UI.div #+ [string "Placed tiles: "]
  placedTilesBox <- tileTable placedTilesBehavior
  UI.div #. "placedTilesWrap"
    #+ [element descriptionString, element placedTilesBox]

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
