
data Player = Red | Black | Empty deriving (Show, Eq)
type Column = [Player]
type Game = [Column]
type GameState = (Player, Game)
type Winner = Player
type Move = Int


-- stuff the needs to be done
-- make game contain current player
-- diagonal win check
-- add current player to makeMove 
-- check legal moves (Done)

-- Define data types or type aliases for a player, game state, move, and winner. (Done)

-- Be able to determine who has won the game state, if anyone. (Done)

-- Be able to compute the result of making a legal move in a game state. (Done)

-- Be able to compute the legal moves from a game state. (Done)

-- (If time) Be able to pretty-print a game into a string. (Done)

makeGameState :: Int -> Int -> GameState -- makes a blank game of size n x m
makeGameState n m = (Red, [[Empty | x <- [1..n]] | y <- [1..m]])

playerToChar :: Player -> Char -- converts player to char
playerToChar Red = 'R'
playerToChar Black = 'B'
playerToChar Empty = '.'

-- move functions

dexColNum :: Game -> Move -> Column -- finds the nth column in a game
dexColNum gm 1 = head gm
dexColNum gm n = dexColNum (tail gm) (n-1)

colHasEmpty :: Column -> Bool -- checks if a column has an empty spot
colHasEmpty [] = False
colHasEmpty (x:xs) = if x == Empty then True else colHasEmpty xs

insertPlay :: Column -> Player -> Column -- changes the last empty spot in a column to a player
insertPlay (Empty:rest) color = aux rest
   where aux (Empty:rest) = Empty:(aux rest)
         aux column = (color):column

legalMove :: Game -> Move -> Bool -- checks if a move is legal
legalMove gm n = 
    if n > length gm || n < 1 then False 
    else if colHasEmpty(dexColNum gm n) then True 
    else False

makeMove :: Move -> GameState -> GameState -- makes a move in a game
makeMove mv gmSt = ((switchPlayer ply), (frntGm ++ [insertPlay (head bkGm) ply] ++ (tail bkGm)))
    where gm = snd gmSt
          ply = fst gmSt
          num = mv-1
          bkGm = drop num gm
          frntGm = take num gm

-- orentation functions

rotateGame :: Game -> Game -- rotates a game 90 degrees the other way
rotateGame gm = if length (last gm) == 0 then [] else (map head gm) : rotateGame (map tail gm)

gameToString :: Game -> String -- converts a game to a string
gameToString gm = if length gm == 0 then [] else function
    where function = (map playerToChar (head gm)) ++ "\n" ++ gameToString (tail gm)

intToChar :: Int -> Char -- converts an int to a char
intToChar n = head (show n)

-- pretty print functions
bars :: Char -> String -- adds bars to a string
bars n = "|" ++ [n]

prettyPrintGame :: Game -> String -- pretty prints a game
prettyPrintGame gm = numString ++ "--" ++ barString ++ (init gameString)
    where width = length gm
          height = length (head gm)
          nGm = rotateGame gm
          nums = [" " ++ [intToChar x] | x <- [1..width]]
          bar = ["--" | x <- [1..width]]
          game = (map playerToChar (head nGm)) ++ "\n" ++ gameToString (tail nGm)
          gameBars = [bars x | x <- game]
          numString = concat nums ++ "\n"
          barString = concat bar ++ "\n"
          gameString = concat gameBars

printGame :: Game -> String -- prints a game
printGame gm = gameToString (rotateGame gm)

displayGame :: Game -> IO () -- displays a game
displayGame gm = putStrLn (prettyPrintGame gm) --putStrLn (printGm gm) -- Non pretty print

--player logic

switchPlayer :: Player -> Player -- switches player
switchPlayer Red = Black
switchPlayer Black = Red

changePlayer :: GameState -> GameState -- changes the player in a game state
changePlayer gmSt = (switchPlayer (fst gmSt), snd gmSt)

isSubString :: String -> String -> Bool -- checks if a string is a substring of another string
isSubString [] _ = True
isSubString _ [] = False
isSubString (x:xs) (y:ys) = if x == y then isSubString xs ys else isSubString (x:xs) ys -- dose not check for spaces

colToString :: Column -> String -- converts a column to a string
colToString col = (map playerToChar col)

-- Winner logic


checkStraightWin :: Game -> Player -> Bool -- checks if a player has won in a straight line
checkStraightWin gm ply = any (fourInRow ply) gm

fourInRow :: Eq a => a -> [a] -> Bool -- checks if there are 4 of the same element in a row
fourInRow elem lst = aux lst 0
  where
    aux [] count = count >= 4
    aux _ 4 = True
    aux (x:xs) count
      | x == elem = aux xs (count + 1)
      | otherwise = aux xs 0

-- transposeGame :: Game -> Game -- allows checkStraightWin to read the columns
-- transposeGame [] = []
-- transposeGame ([]:_) = []
-- transposeGame gm = map head gm : transposeGame (map tail gm)


-- checkHorizontalWin :: Game -> Player -> Bool -- checks if a horizontal win has occured
-- checkHorizontalWin game player = any (isSubString playerString) $ map colToString game
--   where
--     playerString = replicate 4 (playerToChar player)

diagonals1 :: Game -> [[Player]] -- Check this type of diagonal (/)
diagonals1 game = [diag game (x, y) | y <- [0..height-1], x <- [0..width-1], x <= y]
  where height = length game
        width = length (head game)
        diag g (i, j)
          | i < width && j >= 0 = (g !! j !! i) : diag g (i+1, j-1)
          | otherwise = []

diagonals2 :: Game -> [[Player]] -- Check this type of diagonal (\)
diagonals2 game = [diag game (x, y) | y <- [0..height-1], x <- [0..width-1], x + y < height]
  where height = length game
        width = length (head game)
        diag g (i, j)
          | i < width && j < height = (g !! j !! i) : diag g (i+1, j+1)
          | otherwise = []

-- Check if any diagonal contains four of the same player in a row
checkDiagonalWin :: Game -> Player -> Bool
checkDiagonalWin game player = any (isSubString playerString) $ map (map playerToChar) (diagonals1 game ++ diagonals2 game)
  where playerString = replicate 4 (playerToChar player)

checkWin :: Game -> Player -> Bool -- checks if a player has won
checkWin gm ply = checkStraightWin gm ply || checkStraightWin (rotateGame gm) ply || checkDiagonalWin gm ply

winnerOfGame :: Game -> Winner -- returns the winner of a game
winnerOfGame game
  | checkStraightWin game Red || checkDiagonalWin game Red = Red
  | checkStraightWin game Black || checkDiagonalWin game Black = Black
  | otherwise = Empty

-- Game play logic

getAvailableMoves :: GameState -> [Move] -- gets the available moves in a game
getAvailableMoves gmSt = [x | x <- [1..length gm], legalMove gm x]
    where gm = snd gmSt

-- printing logic
playerWon :: Game -> Player -> IO () -- checks if a player has won
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

-- getWinningMoves :: Game -> Player -> [Move] -- gets the winning moves in a game
-- getWinningMoves gm ply = [x | x <- getAvailableMoves gm, checkWin (makeMove x gm) ply]

playGame :: GameState -> IO () -- plays a game
playGame gmSt  = do
    let ply = fst gmSt
        gm = snd gmSt
    displayGame gm

    putStrLn "Enter a column number to make a move"
    col <- getLine
    if (col == "q") then putStrLn "Quitting" 
    
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

-- main :: IO () -- main that asks for number of rows and columns
-- main = do
--     putStrLn "Enter the number of rows"
--     rows <- getLine
--     putStrLn "Enter the number of columns"
--     cols <- getLine
--     let gm = makeGame (read cols) (read rows)
--     playGame gm Red

main :: IO () -- main that uses constant number of rows and columns 
main = playGame (makeGameState 6 7)