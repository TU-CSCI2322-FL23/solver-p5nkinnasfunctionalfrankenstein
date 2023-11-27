import Data.Maybe



data Player = Red | Black deriving (Show, Eq, Read)
type Column = [Maybe Player]
type Game = [Column]
data Winner = Win Player | Tie Player deriving (Show, Eq, Read)
type GameState = (Maybe Player, Game)
--type Winner = Maybe Player
type Move = Int

makeGameState :: Int -> Int -> GameState -- makes a blank game of size n x m
makeGameState n m = (Just Red, [[Nothing | x <- [1..n]] | y <- [1..m]])

playerToChar :: Maybe Player -> Char -- converts player to char
playerToChar (Just Red) = 'R'
playerToChar (Just Black) = 'B'
playerToChar Nothing = '.'

playerToString :: Maybe Player -> String -- converts player to string
playerToString (Just Red) = "Just Red, "
playerToString (Just Black) = "Just Black, "
playerToString Nothing = "Nothing, "

-- move functions

dexColNum :: Game -> Move -> Column -- finds the nth column in a game
dexColNum gm 1 = head gm
dexColNum gm n = dexColNum (tail gm) (n-1)

colHasNothing :: Column -> Bool -- checks if a column has an Nothing spot
colHasNothing [] = False
colHasNothing (x:xs) = (x == Nothing) || colHasNothing xs

insertPlay :: Column -> Maybe Player -> Column -- changes the last Nothing spot in a column to a player
insertPlay (Nothing:rest) color = aux rest
   where aux (Nothing:rest) = Nothing:aux rest
         aux column = color:column

legalMove :: Game -> Move -> Bool -- checks if a move is legal
legalMove gm n =
    not (n > length gm || n < 1) && colHasNothing (dexColNum gm n)

makeMove :: Move -> GameState -> GameState -- makes a move in a game
makeMove mv gmSt = (switchPlayer ply, frntGm ++ [insertPlay (head bkGm) ply] ++ tail bkGm)
    where gm = snd gmSt
          ply = fst gmSt
          num = mv-1
          bkGm = drop num gm
          frntGm = take num gm

-- orentation functions

rotateGame :: Game -> Game -- rotates a game 90 degrees the other way
rotateGame gm = if length (last gm) == 0 then [] else map head gm : rotateGame (map tail gm)

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
prettyPrintGameState (ply, gm) = prettyPrintGame (gm)
printGame :: Game -> String -- prints a game
printGame gm = gameToString (rotateGame gm)

displayGame :: Game -> IO () -- displays a game
displayGame gm = putStrLn (prettyPrintGame gm) --putStrLn (printGm gm) -- Non pretty print

--player logic

switchPlayer :: Maybe Player -> Maybe Player -- switches player
switchPlayer (Just Red) = Just Black
switchPlayer (Just Black) = Just Red

changePlayer :: GameState -> GameState -- changes the player in a game state
changePlayer (ply, gm) = (switchPlayer ply, gm)

isSubString :: String -> String -> Bool -- checks if a string is a substring of another string
isSubString [] _ = True
isSubString _ [] = False
isSubString (x:xs) (y:ys) = if x == y then isSubString xs ys else isSubString (x:xs) ys -- dose not check for spaces

colToString :: Column -> String -- converts a column to a string
colToString = map playerToChar

-- Winner logic


checkStraightWin :: Game -> Maybe Player -> Bool -- checks if a player has won in a straight line
checkStraightWin gm ply = any (fourInRow ply) gm

fourInRow :: Eq a => a -> [a] -> Bool -- checks if there are 4 of the same element in a row
fourInRow elem lst = aux lst 0
  where
    aux [] count = count >= 4
    aux _ 4 = True
    aux (x:xs) count
      | x == elem = aux xs (count + 1)
      | otherwise = aux xs 0

winWithK :: Eq a => a -> [a] -> Int -> Bool
winWithK elem lst k  = aux lst 0
  where
    aux [] count = count >= k
    aux _ k = True
    aux (x:xs) count
      | x == elem = aux xs (count + 1)
      | otherwise = aux xs 0

diagonals1 :: Game -> Game -- Check this type of diagonal (/)
diagonals1 game = [diag game (x, y) | y <- [0..height-1], x <- [0..width-1], x <= y]
  where height = length game
        width = length (head game)
        diag g (i, j)
          | i < width && j >= 0 = (g !! j !! i) : diag g (i+1, j-1)
          | otherwise = []

diagonals2 :: Game -> Game -- Check this type of diagonal (\)
diagonals2 game = [diag game (x, y) | y <- [0..height-1], x <- [0..width-1], x + y < height]
  where height = length game
        width = length (head game)
        diag g (i, j)
          | i < width && j < height = (g !! j !! i) : diag g (i+1, j+1)
          | otherwise = []

checkDiagonalWin :: Game -> Maybe Player -> Bool -- checks if a player has won in a diagonal line
checkDiagonalWin gm ply = any (fourInRow ply) (diagonals1 gm ++ diagonals2 gm)

checkWin :: Game -> Maybe Player -> Bool -- checks if a player has won
checkWin gm ply = checkStraightWin gm ply || checkStraightWin (rotateGame gm) ply || checkDiagonalWin gm ply

winnerOfGame :: Game -> Maybe Winner -- returns the winner of a game
winnerOfGame game
  | checkWin game (Just Red) = Just (Win Red) -- Red Win
  | checkWin game (Just Black) = Just (Win Black) -- Black Win
  | otherwise = Nothing -- Game is ongoing

-- Game play logic

getAvailableMoves :: GameState -> [Move] -- gets the available moves in a game
getAvailableMoves gmSt = [x | x <- [1..length gm], legalMove gm x]
    where gm = snd gmSt

-- printing logic
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



-- HELPER FUNCTION FOR bestMove
evaluateBoard :: GameState -> Int
evaluateBoard gameState@(Just player, game)
  | checkWin game (Just Red) = if player == Red then 1000 else -1000
  | checkWin game (Just Black) = if player == Black then 1000 else -1000
  | otherwise = 0


-- HELPER FUNCTION FOR bestMove
minimax :: GameState -> Int -> Bool -> Int
minimax gameState@(player, game) depth isMaximizingPlayer
  | depth == 0 || isGameDecided (winnerOfGame game) = evaluateBoard gameState
  | isMaximizingPlayer =
      maximum [ minimax (makeMove move gameState) (depth - 1) False | move <- getAvailableMoves gameState, legalMove game move ]
  | otherwise =
      minimum [ minimax (makeMove move gameState) (depth - 1) True | move <- getAvailableMoves gameState, legalMove game move ]
  where
    isGameDecided :: Maybe Winner -> Bool
    isGameDecided Nothing = False
    isGameDecided (Just _) = True

bestMove :: GameState -> Move
bestMove gameState@(Just player, game) =
  let initPlayer = player
      moves = getAvailableMoves gameState
      moveScores = [(minimax (makeMove move gameState) depth (player /= Red), move) | move <- moves]
      bestScore
        | player == Red = maximum moveScores
        | otherwise = minimum moveScores
      depth = 5
  in snd bestScore

-- checkWinWithK :: Int -> GameState -> Bool
-- checkWinWithK k (ply, gm) = any (winWithK ply ((diagonals1 gm) ++ (diagonals2 gm))) k || any (winWithK ply gm) k

-- checkForK :: Int -> GameState -> Move -> Move
-- checkForK _ _ 0 = -1
-- checkForK k (ply, gm) mv = if checkWinWithK k (makeMove mv (ply, gm)) then mv else checkForK k (ply, gm) (mv+1)

-- itK :: Int -> GameState -> Move
-- itK 0 _ = -1
-- itK k (ply, gm) = if rowRes == -1 then itK (k-1) (ply, gm) else rowRes
--   where rowRes = checkForK k (ply, gm) 1

-- bestMove2 :: GameState -> Move 
-- bestMove2 gmSt = if res == -1 then 7 else res 
--   where k = 4
--         res = itK k gmSt 
--         --moves = getAvailableMoves gameState 

whoWillWin :: GameState -> Maybe Winner -- checks who will win -- doesn't account for ties
whoWillWin gmSt = 
  if isNothing (winnerOfGame gm)
  then whoWillWin newGmSt
  else winnerOfGame gm
  where ply = fst gmSt
        gm = snd gmSt
        moves = getAvailableMoves gmSt
        newGmSt = makeMove (bestMove gmSt) gmSt

gameToString2 :: Game -> String -- converts a game to a string
gameToString2 gm = if length gm == 0 then [] else concat function
    where function = map playerToString (head gm) ++ "\n" : [gameToString2 (tail gm)]

testBM :: Int -> GameState
testBM x
    | x == 1 = gm
    | x == 2 = answer
    | x == 3 = gm2
    | x == 4 = answer2
    | x == 5 = gm3
    | x == 6 = answer3
    | x == 7 = gm4
  where
    gm = (Just Red,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Nothing, Nothing, Nothing,Nothing],
              [Just Red, Just Red, Just Red, Nothing, Nothing, Nothing, Nothing]])
    answer = (Just Black,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Nothing, Nothing, Nothing,Nothing],
              [Just Red, Just Red, Just Red, Just Red, Nothing, Nothing,Nothing]])
    gm2 = (Just Red,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Nothing, Nothing, Nothing,Nothing],
              [Just Red, Just Red, Just Red, Just Black, Just Red, Just Red,Just Red]])
    answer2 = (Just Black,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Just Black, Nothing, Nothing, Nothing],
              [Just Red, Just Red, Just Red, Just Black, Just Red, Just Red, Just Red]])
    gm3 = (Just Red,
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Black, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Just Black],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]])
    answer3 = (Just Black,
           [[Nothing, Nothing, Nothing, Nothing, Nothing, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Red, Just Black],
                [Nothing, Nothing, Just Red, Just Black, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Just Black],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]])
    gm4 = (Just Black, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
            [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
            [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
            [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
            [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
            [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
            [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])

bestMoveTest :: GameState -> GameState -> IO()
bestMoveTest gmSt answer =
    if answer == moveMade then putStrLn "Test Passed"
    else do putStrLn "Test Failed"
            putStrLn ("Expected: \n" ++ prettyPrintGame (snd answer))
            putStrLn ("But got: \n" ++ prettyPrintGame (snd moveMade))
    where gmAnswer = snd answer
          moveMade = makeMove (bestMove gmSt) gmSt

multiWinTest :: GameState -> IO()
multiWinTest gmSt =
  if answer1 == moveMade || answer2 == moveMade || answer3 == moveMade || answer4 == moveMade then putStrLn "Test Passed"
  else do putStrLn "Test Failed"
          putStrLn ("Expected: \n" ++ prettyPrintGame (snd answer1))
          putStrLn ("Or: \n"++ prettyPrintGame (snd answer2))
          putStrLn ("Or: \n"++ prettyPrintGame (snd answer3))
          putStrLn ("But got: \n" ++ prettyPrintGame (snd moveMade))

  where moveMade = makeMove (bestMove gmSt) gmSt
        answer1 = (Just Red, [[Nothing, Nothing, Just Black, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])
        answer2 = (Just Red, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Just Black, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])
        answer3 = (Just Red, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Just Black, Just Red, Just Black, Just Red]])
        answer4 = (Just Red, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Just Black, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])
-- bestMoveTest (testBM 1) (testBM 2) -- test for bestMove
runTests :: IO ()
runTests = do
    putStrLn "\nTest 1\n"
    bestMoveTest (testBM 1) (testBM 2)
    putStrLn "\nTest 2\n"
    bestMoveTest (testBM 3) (testBM 4)
    putStrLn "\nTest 3\n"
    bestMoveTest (testBM 5) (testBM 6)
    putStrLn "\nTest 4\n"
    multiWinTest (testBM 7)


readGame :: String -> GameState -- reads a game from a string
readGame str = read str :: GameState


writeGame :: GameState -> FilePath -> IO () -- writes a game to a file
writeGame gm path = writeFile path (show gm)

loadGame :: FilePath -> GameState --untested
loadGame path = do
    contents <- readFile path
    let game = read contents :: GameState
    return game

putBestMove :: GameState -> IO () -- puts the best move in a game -- not done
putBestMove gmSt = do
    let ply = fst gmSt
        gm = snd gmSt
        moves = getAvailableMoves gmSt
        bestMove = head moves
    putStrLn ("The best move is " ++ show bestMove)
    playGame (makeMove bestMove gmSt)

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

        if checkWin newGm ply
        then playerWon newGm ply
        else if getAvailableMoves newGmSt == []
        then gameTie newGmSt
        else playGame newGmSt

play :: IO () -- main that uses constant number of rows and columns 
play = playGame (makeGameState 6 7)

main :: IO ()
main = do -- needs to read a game file and print the winner
  putStrLn "Enter the name of the file you want to load"
  file <- getLine
  putStr(prettyPrintGameState (loadGame file))
  --putStrLn(winnerOfGame (snd (loadGame file))) -- this is not working
  
  