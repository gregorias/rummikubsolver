module Interface.GUI (
  game,
  GUIConfig,
  guiPort,
  guiCssPath,
  defaultGUIConfig,
) where

import Control.Monad
import Game qualified
import Game.Core qualified as Game
import Game.Set qualified as Set
import Game.State (RummikubState)
import Game.State qualified as Game
import Game.TileCountArray qualified as TileCountArray
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Interface.TileChangeCommand (
  TileChangeCommand (..),
  parseTileChangeCommands,
 )
import Reactive.Threepenny qualified as FRP
import Relude hiding (Set, get, on)

data GUIConfig = GUIConfig
  { guiPort :: Int
  , guiCssPath :: String
  }

defaultGUIConfig :: GUIConfig
defaultGUIConfig =
  GUIConfig
    { guiPort = 8080
    , guiCssPath = "resources"
    }

game :: GUIConfig -> IO ()
game config =
  startGUI
    defaultConfig
      { jsPort = Just $ guiPort config
      , jsStatic = Just $ guiCssPath config
      , jsLog = \_ -> return ()
      }
    setup

setup :: Window -> UI ()
setup window = do
  void $ return window # set UI.title "Rummikub Solver"
  UI.addStyleSheet window "main.css"

  windowTitle <- UI.h1 # set text "Rummikub Solver"

  ( userCommandEvent :: FRP.Event (RummikubState -> Either Text RummikubState)
    , userCommandHandler
    ) <-
    liftIO FRP.newEvent
  tableCommandBar <- commandRow "Change table: " Game.modifyTable userCommandHandler
  rackCommandBar <- commandRow "Change rack: " Game.modifyRack userCommandHandler

  -- A state change event. We take in commands and ignore errors.
  let (stateChangeEvent :: FRP.Event (RummikubState -> RummikubState)) =
        userCommandEvent <&> (\command state -> command state & fromRight state)

  -- The authoritative game state.
  stateEvent <- FRP.accumE Game.initialRummikubState stateChangeEvent
  stateBehavior <- FRP.stepper Game.initialRummikubState stateEvent
  let tableBehavior = fmap (TileCountArray.toElemList . Game.table) stateBehavior
      rackBehavior = fmap (TileCountArray.toElemList . Game.rack) stateBehavior

  tableTileTable <- tileTable tableBehavior
  void $ element tableTileTable # set UI.id_ "tableGrid"

  rackTileTable <- tileTable rackBehavior
  void $ element rackTileTable # set UI.id_ "rackGrid"

  tableTileTableWrap <-
    UI.div
      #. "tileTableWrap"
      #+ [element tableTileTable, element tableCommandBar]
  rackTileTableWrap <-
    UI.div
      #. "tileTableWrap"
      #+ [element rackTileTable, element rackCommandBar]

  (solutionEvent, solutionHandler) <- liftIO FRP.newEvent
  void
    $ liftIO
    $ FRP.register
      stateEvent
      ( \stateArg -> do
          maybeSolution <- Game.solveRummikubState stateArg
          maybe (solutionHandler ([], [])) solutionHandler maybeSolution
      )
  solutionBehavior <- FRP.stepper ([], []) solutionEvent
  let solutionSetsBehavior = fmap fst solutionBehavior
      solutionTilesBehavior = fmap snd solutionBehavior

  setsBox <- setsDiv (Set.toTiles <<$>> solutionSetsBehavior)
  placedTilesBox <- placedTilesDiv solutionTilesBehavior

  void
    $ getBody window
    #+ [ element windowTitle
       , element tableTileTableWrap
       , element rackTileTableWrap
       , element setsBox
       , element placedTilesBox
       ]

-- | Create a row with a prompt to modify the state.
commandRow ::
  String ->
  (Int -> Game.Tile -> RummikubState -> Maybe RummikubState) ->
  -- | A handler to act on command events.
  --
  -- The commanded state change can fail with a message.
  FRP.Handler (RummikubState -> Either Text RummikubState) ->
  UI Element
commandRow prompt modifyFunction commandHandler = do
  promptElement <- string prompt
  inputBox <- UI.input
  button <-
    UI.button
      #. "modifyButton"
      # set UI.text "Modify" ::
      UI Element

  on UI.click button
    $ \_ -> do
      inputString <- inputBox # get value :: UI String
      let (tileChangeCommands :: [TileChangeCommand]) =
            fromMaybe [] $ parseTileChangeCommands (fromString inputString)
          (changes :: [RummikubState -> Maybe RummikubState]) = flip map tileChangeCommands $ \case
            Add tile -> modifyFunction 1 tile
            Remove tile -> modifyFunction (-1) tile
          fun state = foldl' (>>=) (Just state) changes
      liftIO $ commandHandler (maybe (Left "Invalid state transition") Right . fun)
  UI.div #+ [element promptElement, element inputBox, element button]

-- | Returns the tile table element.
tileTable ::
  -- | The tiles to display.
  FRP.Behavior [Game.Tile] ->
  UI Element
tileTable tileListBehavior = do
  mainBox <- UI.div #. "tileTable" :: UI Element
  let childrenBehavior :: FRP.Behavior (UI [Element])
      childrenBehavior = fmap tileGrid tileListBehavior
  onChanges
    childrenBehavior
    ( \newChildrenUI -> do
        newChildren <- newChildrenUI
        set children newChildren $ element mainBox
    )
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
tile tileArg =
  UI.div
    #. ("tile " ++ tileColor tileArg)
    # set text (tileText tileArg)
 where
  tileText :: Game.Tile -> String
  tileText Game.Joker = "J"
  tileText (Game.ValueTile (v, _)) = show (fromIntegral v)

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
  onChanges
    setDivBehavior
    ( \newChildrenUI -> do
        newChildren <- sequence newChildrenUI
        set children newChildren $ element setsBox
    )
  UI.div
    #. "setsWrap"
    #+ [element setsString, element setsBox, UI.div #. "clear"]
 where
  setDiv :: [Game.Tile] -> UI Element
  setDiv ss = row (map tile ss) #. "set"

placedTilesDiv :: FRP.Behavior [Game.Tile] -> UI Element
placedTilesDiv placedTilesBehavior = do
  descriptionString <- UI.div #+ [string "Placed tiles: "]
  placedTilesBox <- tileTable placedTilesBehavior
  UI.div
    #. "placedTilesWrap"
    #+ [element descriptionString, element placedTilesBox]
