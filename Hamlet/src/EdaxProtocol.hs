module EdaxProtocol (
    commandLoop,
--    bdFromSfen
) where
-- friends
--import qualified Util
import qualified Piece
import qualified BitBoard
import qualified Move
import qualified Search
import qualified Util

-- GHC

-- libraries

-- std
import System.Exit
import Control.Arrow
--import Data.Array
--import Data.Char
--import Data.Maybe
--import Data.Tuple
--import Text.Regexq
import Text.Regex.Posix

-- Returns true if given string is in the form of acceptable 'move'
isMove :: String -> Bool
isMove str = (length str == 2) && (str =~ "^[a-hA-H][1-8]$" :: Bool)

commandLoop bd = do
    sfens <- getLine
    let cmds = words sfens
    case head cmds of
        "init" -> putStrLn "" >> commandLoop BitBoard.initialBoard
        "quit" -> exitWith $ ExitFailure 1
        "undo" -> putStrLn "undo (Not yet implemented)"
        "redo" -> putStrLn "redo (Not yet implemented)"
        "verbose" ->
            case cmds !! 1 of
                "1" -> (putStrLn $ show bd) >> (putStrLn "\n\n\n")
                "0" -> putStr ""
                _ -> putStr ""
        "go" -> do
            let (Search.Result _ pv) = Search.alphabeta 7 bd
            case head pv of
                Move.Nil -> do
                    putStrLn $ "\n\nHamlet plays PS"
                    commandLoop $ BitBoard.move bd (head pv)
                _ -> do
                    putStrLn $ "\n\nHamlet plays " ++ (show $ head pv)
                    commandLoop $ BitBoard.move bd (head pv)
        _   | isMove $ head cmds -> do
                putStrLn $ "\n\nYou play " ++ (head cmds)
                commandLoop $ BitBoard.moveByPos bd (Util.posFromUSI (head cmds))
            | otherwise -> putStrLn ("undefined command.." ++ sfens)
    commandLoop bd -- next
