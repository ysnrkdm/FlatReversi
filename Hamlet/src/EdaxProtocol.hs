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

-- GHC

-- libraries

-- std
import Control.Arrow
--import Data.Array
--import Data.Char
--import Data.Maybe
--import Data.Tuple
--import Text.Regex
import Text.Regex.Posix

-- Returns true if given string is in the form of acceptable 'move'
isMove :: String -> Bool
isMove str = (length str == 2) && (str =~ "^[a-h][1-8]$" :: Bool)

commandLoop bd = do
    sfens <- getLine
    let cmds = words sfens
    case head cmds of
        "init" -> putStrLn "initialized"
        "quit" -> putStrLn "bye"
        "undo" -> putStrLn "undo"
        "redo" -> putStrLn "redo"
        "go" -> do
--            let (Search.Result _ pv) = Search.minmax 1 bd
--            putStrLn $ "bestmove " ++ Move.mvToUSI (head pv)
            putStrLn "thought. but nothing comes up in my mind..."
        _   | isMove $ head cmds -> do
                putStrLn $ "move to " ++ (head cmds)
--                let (bd2, mvs) = readUSIPosition sfens
--                    bd3 = Board.bdDoMvs bd2 mvs
                commandLoop bd
            | otherwise -> putStrLn ("undefined command.." ++ sfens)
    commandLoop bd -- next
