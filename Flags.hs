module Flags where


import System.Environment
import System.Directory  
import System.IO  
import Data.List  


--flags
-- -h help
-- -w winner
-- -d <num> depth
-- -m <move> move
-- -v verbose
-- -i interactive


import System.Environment

import LoadAll


main :: IO ()
main = do
    args <- getArgs
    processArgs args

verboseOutput :: String -> String -> IO ()
verboseOutput file move = do
    let game = loadGame file

    putStrLn $ "Move: " ++ move
    putStrLn $ "Rating: " ++ rating

makeMoveOutput :: String -> String -> IO ()
makeMoveOutput file move = do
    let game = loadGame file
    -- Here you should implement the logic to make the move and print the resulting board
    putStrLn $ "Move made: " ++ move
    putStrLn "Resulting board: "
    prettyPrintGameState (makeMove move game) 
    -- printBoard board

winnerOutput :: String -> IO ()
winnerOutput file = do
    let game = loadGame file
        winner = whoWillWin game
        in putStrLn $ "Winner: " ++ winner

loadAndPlayOutput :: String -> IO ()
loadAndPlayOutput file = do
    let game = loadGame file
    putStrLn $ "Load file: " ++ file
    playGame game

helpOutput :: IO ()
helpOutput = do
    putStrLn "Help: "
    putStrLn "  -h help"
    putStrLn "  -w winner"
    putStrLn "  -d <num> depth"
    putStrLn "  -m <move> move"
    putStrLn "  -v verbose"
    putStrLn "  -i interactive"
    

interactiveOutput :: IO ()
interactiveOutput = play()

processArgs :: [String] -> IO ()
processArgs [] = return ()
processArgs ("-h") = do
    usage
processArgs ("-w":file) = do
    putStrLn "Winner flag activated"
    winnerOutput file
processArgs ("-d":file) = do
    putStrLn $ "Depth flag activated with depth " ++ x
processArgs ("-m":file:move) = do
    makeMoveOutput file move
processArgs ("--move":file:move) = do
    makeMoveOutput file move
processArgs ("-v":file:move) = do
    verboseOutput file move
processArgs ("--verbose":file:move) = do
    verboseOutput file move
processArgs ("-i") = do
    putStrLn "Interactive flag activated"
    interactiveOutput
processArgs ("--interactive") = do
    putStrLn "Interactive flag activated"
    interactiveOutput
processArgs ("-i":file) = do
    putStrLn "Interactive flag activated"
    loadAndPlayOutput file
processArgs ("--interactive":file) = do
    putStrLn "Interactive flag activated"
    loadAndPlayOutput file
processArgs (x:xs) = do
    putStrLn $ "Unknown flag: " ++ x






-- usage :: IO ()
-- usage = do
--     putStrLn "Usage: connect4 [flags]"
--     putStrLn "Flags:"
--     putStrLn "  -h help"
--     putStrLn "  -w winner"
--     putStrLn "  -d <num> depth"
--     putStrLn "  -m <move> move"
--     putStrLn "  -v verbose"
--     putStrLn "  -i interactive"


