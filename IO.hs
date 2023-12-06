module IO where
import Connect4
import Win




playerToChar :: Maybe Player -> Char -- converts player to char
playerToChar (Just Red) = 'R'
playerToChar (Just Black) = 'B'
playerToChar Nothing = '.'

playerToString :: Maybe Player -> String -- converts player to string
playerToString (Just Red) = "Just Red, "
playerToString (Just Black) = "Just Black, "
playerToString Nothing = "Nothing, "

gameToString2 :: Game -> String -- converts a game to a string
gameToString2 gm = if length gm == 0 then [] else concat function
    where function = map playerToString (head gm) ++ "\n" : [gameToString2 (tail gm)]

gameToString :: Game -> String -- converts a game to a string
gameToString gm = if length gm == 0 then [] else function
    where function = map playerToChar (head gm) ++ "\n" ++ gameToString (tail gm)

intToChar :: Int -> Char -- converts an int to a char
intToChar n = head (show n)

-- pretty print functions
bars :: Char -> String -- adds bars to a string
bars n = "|" ++ [n]

prettyPrintGame :: Game -> String -- pretty prints a game
prettyPrintGame gm = numString ++ "--" ++ barString ++ init gameString
    where width = length gm
          height = length (head gm)
          nGm = rotateGame gm
          nums = [" " ++ [intToChar x] | x <- [1..width]]
          bar = ["--" | x <- [1..width]]
          game = map playerToChar (head nGm) ++ "\n" ++ gameToString (tail nGm)
          gameBars = [bars x | x <- game]
          numString = concat nums ++ "\n"
          barString = concat bar ++ "\n"
          gameString = concat gameBars

prettyPrintGameState :: GameState -> String -- pretty prints a game state
prettyPrintGameState (ply, gm) = prettyPrintGame gm


isSubString :: String -> String -> Bool -- checks if a string is a substring of another string
isSubString [] _ = True
isSubString _ [] = False
isSubString (x:xs) (y:ys) = if x == y then isSubString xs ys else isSubString (x:xs) ys -- dose not check for spaces

colToString :: Column -> String -- converts a column to a string
colToString = map playerToChar

printGame :: Game -> String -- prints a game
printGame gm = gameToString (rotateGame gm)

displayGame :: Game -> IO () -- displays a game
displayGame gm = putStrLn (prettyPrintGame gm) --putStrLn (printGm gm) -- Non pretty print


readGame :: String -> GameState -- reads a game from a string
readGame str = read str :: GameState


writeGame :: GameState -> FilePath -> IO () -- writes a game to a file
writeGame gm path = writeFile path (show gm)

loadGame :: FilePath -> IO GameState --untested
loadGame path = do
    contents <- readFile path
    let game = read contents :: GameState
    return game

playerWon :: Game -> Maybe Player -> IO () -- checks if a player has won
playerWon gm ply = do
            putStrLn (prettyPrintGame gm ++ "\n===" ++ show ply ++ " wins!===\n")
            putStrLn "Enter q to quit or anything else to play again"
            quit <- getLine
            if quit == "q" then putStrLn "Quitting"
            else playGame (makeGameState (length (head gm)) (length gm))

gameTie :: GameState -> IO () -- checks if a game is a tie
gameTie gmSt = do
            putStrLn (prettyPrintGame gm ++ "\n===Tie game!===\n")
            putStrLn "Enter q to quit or anything else to play again"
            quit <- getLine
            if quit == "q" then putStrLn "Quitting"
            else playGame (makeGameState (length (head gm)) (length gm))
    where gm = snd gmSt

playGame :: GameState -> IO () -- plays a game
playGame gmSt  = do
    let ply = fst gmSt
        gm = snd gmSt
    displayGame gm

    putStrLn "Enter a column number to make a move"
    col <- getLine
    if col == "q" then putStrLn "Quitting"

    else if col == "g" then putStrLn (gameToString2 gm)

    else if not (legalMove gm (read col)) then do
        putStrLn "------Illegal move------"
        playGame gmSt
    else do
        let newGmSt = makeMove (read col) gmSt
            newGm = snd newGmSt
            newPly = fst newGmSt

        if checkWin newGm (Just ply)
        then playerWon newGm (Just ply)
        else if getAvailableMoves newGmSt == []
        then gameTie newGmSt
        else playGame newGmSt

play :: IO () -- main that uses constant number of rows and columns 
play = playGame (makeGameState 6 7)