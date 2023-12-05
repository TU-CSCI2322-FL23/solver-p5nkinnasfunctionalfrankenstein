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
import Connect4
import Win
import IO
import Main
import Solver
import Tests



-- still need rateGame
-- also need to convert IO gmst to gmst

stringToMove :: String -> Move
stringToMove str = read str :: Move



main :: IO ()
main = do
    args <- getArgs
    processArgs args

verboseOutput :: String -> String -> IO ()
verboseOutput file move = do
    let game = loadGame file
        rating = rateGame game
    putStrLn $ "Move: " ++ move
    putStrLn $ "Rating: " ++ rating

makeMoveOutput :: String -> String -> IO ()
makeMoveOutput file move = do
    let game = loadGame file
    -- Here you should implement the logic to make the move and print the resulting board
    putStrLn $ "Move made: " ++ move
    putStrLn "Resulting board: "
    putStrLn (prettyPrintGameState (makeMove (stringToMove move) game))
    -- printBoard board

winnerOutput :: String -> IO ()
winnerOutput file = do
    let game = loadGame file
        winner = whoWillWin game
        in putStrLn $ "Winner: " ++ winnerToString winner

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
    putStrLn "  -p <file> play file"
    

processArgs :: [String] -> IO ()
processArgs [] = return ()
processArgs ("-h") = do
    helpOutput
processArgs ("-w":file) = do
    putStrLn "Winner flag activated"
    winnerOutput file
processArgs ("-d":depth:file) = do
    putStrLn $ "Depth flag activated with depth " ++ depth
processArgs ("-m":file:move:xs) = do
    makeMoveOutput file move
processArgs ("--move":file:move:xs) = do
    makeMoveOutput file move
processArgs ("-v":file:move:xs) = do
    verboseOutput file move
processArgs ("--verbose":file:move:xs) = do
    verboseOutput file move
processArgs ("-i":xs) = do
    putStrLn "Interactive flag activated"
    play
processArgs ("--interactive":xs) = do
    putStrLn "Interactive flag activated"
    play
processArgs ("-p":file:xs) = do
    putStrLn "Interactive flag activated"
    loadAndPlayOutput file
processArgs ("--playFile":file:xs) = do
    putStrLn "Interactive flag activated"
    loadAndPlayOutput file
processArgs (x:xs) = do
    putStrLn $ "Unknown flag: " ++ x

