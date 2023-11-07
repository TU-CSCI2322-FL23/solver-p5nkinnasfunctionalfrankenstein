
data Player = Red | Black | Empty deriving (Show, Eq)
type Column = [Player]
type Game = [Column]
type GameState = (Player, Game)
type Winner = Player
type Move = Int

-- class GameState a where 
--     winnerOfGame :: a -> Winner
--     makeMove :: Move -> a -> Player -> a
--     getAvailableMoves :: a -> [Move]
--     getWinningMoves :: a -> Player -> [Move]
--     playGame :: a -> Player -> IO ()
--     game :: Game


-- stuff the needs to be done
-- make game contain current player
-- diagonal win check
-- add current player to makeMove
-- check legal moves (Done)

-- Define data types or type aliases for a player, game state, move, and winner. 

-- Be able to determine who has won the game state, if anyone. (In progress)

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


-- insertPlay :: Column -> Player -> Column -> Column -- changes the last empty spot in a column to a player
-- insertPlay [] ply backEnd = backEnd
-- insertPlay frontEnd ply backEnd = 
--     if last frontEnd == Empty 
--         then (init frontEnd) ++ [ply] ++ backEnd 
--         else insertPlay (init frontEnd) ply ([last frontEnd] ++ backEnd)
insertPlay :: Column -> Player -> Column -- changes the last empty spot in a column to a player
insertPlay (Empty:rest) color = aux rest
   where aux (Empty:rest) = Empty:(aux rest)
         aux column = (color):column

backGame :: Game -> Move -> Game -- finds the back set of columns in a game based on a move (inculding the changing column)
backGame gm n = if n == 1 then gm else backGame (tail gm) (n-1)

frontGame :: Game -> Move -> Game -- finds the front set of columns in a game based on a move (not inculding the changing column)
frontGame gm n = if n == length gm then init gm else frontGame (init gm) (n)

legalMove :: Game -> Move -> Bool -- checks if a move is legal
legalMove gm n = 
    if n > length gm || n < 1 then False 
    else if colHasEmpty(dexColNum gm n) then True 
    else False

makeMove :: Move -> GameState -> GameState -- makes a move in a game
makeMove mv gmSt = ((switchPlayer ply), (frntGm ++ [insertPlay (head bkGm) ply] ++ (tail bkGm)))
    where gm = snd gmSt
          ply = fst gmSt
          num = mv - 1
          bkGm = drop num gm
          frntGm = take num gm

-- orentation functions

rotateGame :: Game -> Game -- rotates a game 90 degrees
rotateGame gm = if length (head gm) == 0 then [] else (map last gm) : rotateGame (map init gm)

rotateGame2 :: Game -> Game -- rotates a game 90 degrees the other way
rotateGame2 gm = if length (last gm) == 0 then [] else (map head gm) : rotateGame2 (map tail gm)

flipHorizontal :: Game -> Game -- flips a game horizontally
flipHorizontal gm = if length gm == 0 then [] else (reverse (head gm)) : flipHorizontal (tail gm)

gameToString :: Game -> String -- converts a game to a string
gameToString gm = if length gm == 0 then [] else function
    where function = (map playerToChar (head gm)) ++ "\n" ++ gameToString (tail gm)

intToChar :: Int -> Char -- converts an int to a char
intToChar n = head (show n)

-- pretty print functions

bars2 :: Char -> String -- adds bars to a string
bars2 n = "|" ++ [n] ++ "|"

prettyPrintGame2 :: Game -> String -- pretty prints a game
prettyPrintGame2 gm = " " ++ numString ++ "---" ++ barString ++ "|" ++ (init gameString)
    where width = length (head gm)
          height = length gm
          nGm = rotateGame2 gm
          nums = [" " ++ [intToChar x] ++ " " | x <- [1..width]]
          bar = ["---" | x <- [1..width]]
          game = (map playerToChar (head nGm)) ++ "\n" ++ gameToString (tail nGm)
          gameBars = [bars2 x | x <- game]
          numString = concat nums ++ "\n"
          barString = concat bar ++ "\n"
          gameString = concat gameBars


bars :: Char -> String -- adds bars to a string
bars n = "|" ++ [n] -- ++ "|"

prettyPrintGame :: GameState -> String -- pretty prints a game
prettyPrintGame gmSt = numString ++ "--" ++ barString ++ (init gameString)
    where gm = snd gmSt
          width = length gm
          height = length (head gm)
          nGm = rotateGame2 gm
          nums = [" " ++ [intToChar x] | x <- [1..width]]
          bar = ["--" | x <- [1..width]]
          game = (map playerToChar (head nGm)) ++ "\n" ++ gameToString (tail nGm)
          gameBars = [bars x | x <- game]
          numString = concat nums ++ "\n"
          barString = concat bar ++ "\n"
          gameString = concat gameBars

printGame :: Game -> String -- prints a game
printGame gm = gameToString (rotateGame2 gm)

displayGame :: GameState -> IO () -- displays a game
displayGame gm = putStrLn (prettyPrintGame gm) --putStrLn (printGm gm) -- Non pretty print

colToString :: Column -> String -- converts a column to a string
colToString col = (map playerToChar col)

--player logic

switchPlayer :: Player -> Player -- switches player
switchPlayer Red = Black
switchPlayer Black = Red

isSubString :: String -> String -> Bool -- checks if a string is a substring of another string
isSubString [] _ = True
isSubString _ [] = False
isSubString (x:xs) (y:ys) = if x == y then isSubString xs ys else isSubString (x:xs) ys -- dose not check for spaces

-- Winner logic

checkStraightWin :: Game -> Player -> Bool -- checks if a player has won in a straight line
checkStraightWin gm ply = [True | x <- strLst, isSubString plyStr x] /= []
    where strLst = map colToString gm
          plyStr = [playerToChar ply | x <- [1..4]] 

checkDiagonalWin :: Game -> Player -> Bool -- checks if a player has won diagonally
checkDiagonalWin gm ply = undefined

checkWin :: Game -> Player -> Bool -- checks if a player has won
checkWin gm ply = checkStraightWin (rotateGame gm) ply || checkStraightWin gm ply -- || checkDiagonalWin gm ply


winnerOfGame :: Game -> Winner -- returns the winner of a game
winnerOfGame gm = if checkWin gm Red then Red else if checkWin gm Black then Black else Empty

-- Game play logic

getAvailableMoves :: GameState -> [Move] -- gets the available moves in a game
getAvailableMoves gmSt = [x | x <- [1..length gm], legalMove gm x]
    where gm = snd gmSt

-- getWinningMoves :: Game -> Player -> [Move] -- gets the winning moves in a game
-- getWinningMoves gm ply = [x | x <- getAvailableMoves gm, checkWin (makeMove x gm) ply]

playGame :: GameState -> IO () -- plays a game
playGame gmSt  = do
    let ply = fst gmSt
    let gm = snd gmSt
    displayGame gmSt
    putStrLn "Enter a column number to make a move"
    col <- getLine
    if (col == "q") then putStrLn "Quitting" 
    else if not (legalMove gm (read col)) then do
        putStrLn "------Illegal move------"
        playGame gmSt
    else do
        let newGm = makeMove (read col) gmSt
        if checkWin gm ply
        then do 
            putStrLn (prettyPrintGame newGm ++ "\n===" ++ show ply ++ " wins!===\n")
            putStrLn "Enter q to quit or anything else to play again"
            quit <- getLine
            if quit == "q" then putStrLn "Quitting"
            else playGame (makeGameState (length (head gm)) (length gm))
        else if getAvailableMoves newGm == [] 
        then do 
            putStrLn (prettyPrintGame newGm ++ "\n===Tie game!===\n") 
            putStrLn "Enter q to quit or anything else to play again"
            quit <- getLine
            if quit == "q" then putStrLn "Quitting"
            else playGame (makeGameState (length (head gm)) (length gm)) 
        else playGame newGm
        --playGame newGm (switchPlayer ply)

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