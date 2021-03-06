module Interface.GUI (
    game
    , GUIConfig
    , guiPort
    , guiCssPath
    , defaultGUIConfig
    ) where

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Reactive.Threepenny as FRP

import qualified Game as Game
import Interface.Common

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
  return window # set UI.title "rummikubsolver"
  UI.addStyleSheet window "main.css"

  windowTitle <- UI.h1 # set text "rummikubsolver"

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
    (\stateArg -> do
      maybeSolution <- Game.solveRummikubState stateArg
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
    element windowTitle
    , element tableTileTableWrap
    , element rackTileTableWrap
    , element setsBox
    , element placedTilesBox
    ]
  return ()

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
        modifyMaybeSafe modifyFun state =
          maybe state id $ modifyFun state
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
