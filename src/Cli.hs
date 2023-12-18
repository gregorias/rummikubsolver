-- The main module containing the CLI of launcher.
module Cli (main) where

import Control.Applicative
import Game.State (initialRummikubState)
import Interface.Console (game)
import Interface.GUI qualified as GUI
import Options.Applicative (
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  subparser,
  value,
 )
import Options.Applicative qualified
import Relude

data Config
  = Cli
  | Gui ConfigGui

data ConfigGui = ConfigGui {cgPort :: !Int, cgCss :: !Text}

defaultPort :: Int
defaultPort = 8080

defaultCss :: Text
defaultCss = "resources"

guiP :: Options.Applicative.Parser ConfigGui
guiP =
  ConfigGui
    <$> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help ("The port under which the GUI web interface will run.\ndefault=" <> show defaultPort)
          <> value defaultPort
      )
    <*> option
      auto
      ( long "css"
          <> metavar "PATH"
          <> help ("The directory containing the css/main.css file.\ndefault=" <> toString defaultCss)
          <> value defaultCss
      )

cliP :: Options.Applicative.Parser Config
cliP =
  subparser
    ( command "gui" (info ((Gui <$> guiP) <**> helper) (progDesc "Runs the GUI interface"))
        <> command "cli" (info (pure Cli) (progDesc "Runs the CLI interface"))
    )
    <|> pure (Gui (ConfigGui defaultPort defaultCss))

cliInfo :: Options.Applicative.InfoMod a
cliInfo =
  fullDesc
    <> header "Rummikub Solver"
    <> progDesc "Finds the biggest moves in Rummikub."

main :: IO ()
main = do
  config <- execParser (info (helper <*> cliP) cliInfo)
  case config of
    Cli -> evalStateT game initialRummikubState
    Gui (ConfigGui{cgPort = port, cgCss = css}) ->
      GUI.game GUI.defaultGUIConfig{GUI.guiPort = port, GUI.guiCssPath = toString css}
