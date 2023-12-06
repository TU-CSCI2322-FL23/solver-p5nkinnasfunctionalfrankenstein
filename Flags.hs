module Flags where
import System.Directory 
import System.IO 
import Data.List 
import System.Environment
import Connect4
import Win
import IO
import Main
import Solver
import Tests

--flags
-- -h help
-- -w winner
-- -d <num> depth
-- -m <move> move
-- -v verbose
-- -i interactive




ioConv :: IO GameState -> GameState
ioConv io = io


-- still need rateGame
-- also need to convert IO gmst to gmst

stringToMove :: String -> Move
stringToMove str = read str :: Move



main :: IO () --might still need to modify load game
main = do
    args <- getArgs
    game <- loadGame
    processArgs args game

verboseOutput :: GameState -> Move -> IO ()
verboseOutput game move = do
        rating = rateGame game
    putStrLn $ "Move: " ++ move
    putStrLn $ "Rating: " ++ rating

makeMoveOutput :: GameState -> Move -> IO ()
makeMoveOutput game move = do
    -- Here you should implement the logic to make the move and print the resulting board
    putStrLn $ "Move made: " ++ move
    putStrLn "Resulting board: "
    putStrLn (prettyPrintGameState (makeMove (stringToMove move) game))
    -- printBoard board

winnerOutput :: GameState -> IO ()
winnerOutput game = do
        winner = whoWillWin game
        in putStrLn $ "Winner: " ++ winnerToString winner

loadAndPlayOutput :: GameState -> IO ()
loadAndPlayOutput game = 
    do playGame game

depthOutput :: String -> String -> IO ()
depthOutput depth file = do
    let game = loadGame file
        rating = rateGame game
    putStrLn $ "Depth: " ++ depth
    putStrLn $ "Rating: " ++ rating

helpOutput :: IO ()
helpOutput = do
    putStrLn "Help: "
    putStrLn "  -h help"
    putStrLn "  -w winner"
    putStrLn "  -d <num> depth"
    putStrLn "  -m <move> move"
    putStrLn "  -v verbose"
    putStrLn "  -i interactive"
    putStrLn "  -p <file> play file"


-- processArgs :: [String] -> IO ()
-- processArgs [] = return ()
-- processArgs ("-h") = do
--     helpOutput
-- processArgs ("-w":file) = do
--     putStrLn "Winner flag activated"
--     winnerOutput file
-- processArgs ("-d":depth:file) = do
--     putStrLn $ "Depth flag activated with depth " ++ depth
-- processArgs ("-m":file:move:xs) = do
--     makeMoveOutput file move
-- processArgs ("--move":file:move:xs) = do
--     makeMoveOutput file move
-- processArgs ("-v":file:move:xs) = do
--     verboseOutput file move
-- processArgs ("--verbose":file:move:xs) = do
--     verboseOutput file move
-- processArgs ("-i":xs) = do
--     putStrLn "Interactive flag activated"
--     play
-- processArgs ("--interactive":xs) = do
--     putStrLn "Interactive flag activated"
--     play
-- processArgs ("-p":file:xs) = do
--     putStrLn "Interactive flag activated"
--     loadAndPlayOutput file
-- processArgs ("--playFile":file:xs) = do
--     putStrLn "Interactive flag activated"
--     loadAndPlayOutput file
-- processArgs (x:xs) = do
--     putStrLn $ "Unknown flag: " ++ x



processArgs :: [String] -> GameState -> IO ()
processArgs [] _ = putStrLn "No arguments provided. Use -h for help."
processArgs (flag:args) game = case normalizeFlag flag of
    "-h" -> helpOutput
    "-w" -> case args of
              (file:_) -> winnerOutput game
              _ -> putStrLn "Missing file argument for -w"
    "-d" -> case args of
              (depth:file:_) -> depthOutput depth file
              _ -> putStrLn "Missing arguments for -d"
    "-m" -> case args of
              (file:move:_) -> makeMoveOutput game move
              _ -> putStrLn "Missing arguments for -m"
    "-v" -> case args of
              (file:move:_) -> verboseOutput game move
              _ -> putStrLn "Missing arguments for -v"
    "-i" -> play -- Assuming interactiveOutput does not need args
    "-p" -> case args of
              (file:_) -> loadAndPlayOutput game
              _ -> putStrLn "Missing file argument for -p"
    _    -> putStrLn $ "Unknown flag: " ++ flag

normalizeFlag :: String -> String
normalizeFlag flag
  | "--" `isPrefixOf` flag = drop 2 flag
  | otherwise              = flag


