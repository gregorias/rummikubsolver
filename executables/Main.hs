import Control.Applicative
import Control.Monad.State (evalStateT)
import Data.List
import Data.Maybe
import qualified Safe
import System.Environment

import Game
import Interface.Console (game)
import qualified Interface.GUI as GUI


helpOptions :: [String]
helpOptions = ["-h", "--help"]

cliOption :: String
cliOption = "cli"

guiOption :: String
guiOption = "gui"

portOption :: String
portOption = "port"

cssOption :: String
cssOption = "css"

optionNames :: [String]
optionNames = helpOptions ++ [cliOption, guiOption, portOption, cssOption]

helpString :: String
helpString = 
  "Usage: RummikubSolver [OPTION]...\n" ++
  "RummikubSolver finds the biggest possible move in a Rummikub game.\n" ++
  "RummikubSolver accepts the following options:\n" ++ 
  "    -h|--help - Display this help message.\n" ++
  "    --gui|--cli - Run the gui/console interface (--gui is on default).\n" ++
  "    --port=PORT - The port under which the GUI web interface will run \n" ++
  "                  (default=8080).\n" ++
  "    --css=PATH - The directory containing css/main.css file.\n" ++
  "                 (default=resources)."

defaultPort :: Int
defaultPort = 8080

defaultCssPath :: String
defaultCssPath = "resources"
  
main :: IO ()
main = do
  args <- getArgs
  if or $ map elem helpOptions <*> [args]
  then putStrLn helpString
  else
    let
      parsedArgsMaybe = map parseArg args
      unparsedArgs = map fst . filter (isNothing . snd) . zip args 
        $ parsedArgsMaybe
      parsedArgs = catMaybes parsedArgsMaybe
      unknownOptions = filter (not . flip elem optionNames) 
        . map getOptionName 
        $ parsedArgs
    in
      if not $ null unparsedArgs
      then putStrLn $ "Could not parse following options: " ++
        intercalate " " unparsedArgs
      else if not $ null unknownOptions
        then putStrLn $ "Unknown options have been provided: " ++
          intercalate " " unknownOptions
        else if elem (NoArgOption "cli") parsedArgs
          then evalStateT game initialRummikubState
          else runGUI parsedArgs

runGUI :: [Option] -> IO ()
runGUI options = GUI.game
  GUI.defaultGUIConfig {
    GUI.guiPort = port
    , GUI.guiCssPath = css
    }
  where 
    port = maybe defaultPort id
      . fmap (Safe.readDef 8080)
      . Safe.headMay
      . map getOptionValue
      . filter isArgOption
      . filter ((== portOption) . getOptionName) 
      $ options
    css = maybe defaultCssPath id
      . Safe.headMay
      . map getOptionValue
      . filter isArgOption
      . filter ((== cssOption) . getOptionName) 
      $ options
  
    
data Option = NoArgOption String | ArgOption String String deriving (Eq, Show)

isArgOption :: Option-> Bool
isArgOption (NoArgOption _) = False
isArgOption (ArgOption _ _) = True

getOptionName :: Option -> String
getOptionName (NoArgOption v) = v
getOptionName (ArgOption v _) = v

getOptionValue :: Option -> String
getOptionValue (NoArgOption _) = ""
getOptionValue (ArgOption _ v) = v

parseArg :: String -> Maybe Option
parseArg arg = 
  if not $ take 2 arg == "--"
  then Nothing
  else
    let
      headlessArg = drop 2 arg
    in
      if elem '=' arg
      then
        let 
          (name, value) = break (== '=') headlessArg
        in
          Just $ ArgOption name (tail value)
      else
        Just $ NoArgOption headlessArg
