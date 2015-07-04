import Control.Monad.State (evalStateT)
import Game
import Interface.Console (game)

main :: IO ()
main = evalStateT game initialRummikubState
