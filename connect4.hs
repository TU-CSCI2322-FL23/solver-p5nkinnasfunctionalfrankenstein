
data Player = Red | Black | Empty deriving (Show, Eq)
type Game = [[Player]]
type Winner = Player
-- stuff the needs to be done
-- make game contain current player
-- diagonal win check
-- add current player to makeMove
-- check legal moves

-- Define data types or type aliases for a player, game state, move, and winner.

-- Be able to determine who has won the game state, if anyone.

-- Be able to compute the result of making a legal move in a game state.

-- Be able to compute the legal moves from a game state.

-- (If time) Be able to pretty-print a game into a string.

makeGame :: Int -> Int -> Game -- makes a blank game of size n x m
makeGame n m = [[Empty | x <- [1..n]] | y <- [1..m]]

playerToChar :: Player -> Char -- converts player to char
playerToChar Red = 'R'
playerToChar Black = 'B'
playerToChar Empty = '.'

-- move functions

findColNum :: Game -> [Player] -> Int -> [Player] -- finds the nth column in a game
findColNum gm backEnd n = if n == 1 then (head gm) ++ backEnd else findColNum (tail gm) ((head gm) ++ backEnd) (n-1)

insertPlay :: [Player] -> Player -> [Player] -> [Player] -- changes the last empty spot in a column to a player
insertPlay [] ply backEnd = backEnd
insertPlay frontEnd ply backEnd = 
    if last frontEnd == Empty 
        then (init frontEnd) ++ [ply] ++ backEnd 
        else insertPlay (init frontEnd) ply ([last frontEnd] ++ backEnd)

backGame :: Game -> Int -> Game -- finds the nth column in a game
backGame gm n = if n == 1 then gm else backGame (tail gm) (n-1)

frontGame :: Game -> Int -> Game -- finds the nth column in a game
frontGame gm n = if n == length gm then init gm else frontGame (init gm) (n)

canMakeMove :: Int -> Game -> Bool -- checks if a move can be made in a game
canMakeMove n gm = if n > length (head gm) || n < 1 then False else True

makeMove :: Int -> Game -> Player -> Game -- makes a move in a game
makeMove n gm ply = frntGm ++ [insertPlay (head bkGm) ply []] ++ (tail bkGm)
    where bkGm = backGame gm n
          frntGm = frontGame gm n

-- orentation functions

rotateGame :: Game -> Game -- rotates a game 90 degrees
rotateGame gm = if length (head gm) == 0 then [] else (map last gm) : rotateGame (map init gm)

rotateGame2 :: Game -> Game -- rotates a game 90 degrees
rotateGame2 gm = if length (last gm) == 0 then [] else (map head gm) : rotateGame2 (map tail gm)

flipHorizontal :: Game -> Game -- flips a game horizontally
flipHorizontal gm = if length gm == 0 then [] else (reverse (head gm)) : flipHorizontal (tail gm)

gameToString :: Game -> String -- converts a game to a string
gameToString gm = if length gm == 0 then [] else function
    where function = (map playerToChar (head gm)) ++ "\n" ++ gameToString (tail gm)

intToChar :: Int -> Char -- converts an int to a char
intToChar n = head (show n)

bars2 :: Char -> String
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


bars :: Char -> String
bars n = "|" ++ [n] -- ++ "|"

prettyPrintGame :: Game -> String -- pretty prints a game
prettyPrintGame gm = numString ++ "--" ++ barString ++ (init gameString)
    where width = length (head gm)
          height = length gm
          nGm = rotateGame2 gm
          nums = [" " ++ [intToChar x] | x <- [1..width]]
          bar = ["--" | x <- [1..width]]
          game = (map playerToChar (head nGm)) ++ "\n" ++ gameToString (tail nGm)
          gameBars = [bars x | x <- game]
          numString = concat nums ++ "\n"
          barString = concat bar ++ "\n"
          gameString = concat gameBars

displayGame :: Game -> IO () -- displays a game
displayGame gm = putStrLn (prettyPrintGame gm)
    --putStrLn (gameToString (rotateGame2 gm))


--player logic

switchPlayer :: Player -> Player -- switches player
switchPlayer Red = Black
switchPlayer Black = Red

-- game logic

checkStraightWin :: Game -> Player -> Bool -- checks if a player has won in a straight line
checkStraightWin gm ply = if length gm < 4 then False else function
    where function = checkStraightWin (tail gm) ply || checkStraightWin (init gm) ply || checkStraightWin (tail gm) ply || checkStraightWin (init gm) ply

checkDiagonalWin :: Game -> Player -> Bool -- checks if a player has won diagonally
checkDiagonalWin gm ply = undefined

checkWin :: Game -> Player -> Bool -- checks if a player has won
checkWin gm ply = checkStraightWin (rotateGame gm) ply || checkStraightWin gm ply || checkDiagonalWin gm ply

winnerOfGame :: Game -> Winner -- returns the winner of a game
winnerOfGame gm = if checkWin gm Red then Red else if checkWin gm Black then Black else Empty

playGame :: Game -> Player -> IO () -- plays a game
playGame gm ply = do
    displayGame gm
    putStrLn "Enter a column number to make a move"
    col <- getLine
    if (col == "q") then putStrLn "Quitting" 
    else do
        let newGm = makeMove (read col) gm ply
        --if checkWin newGm ply then putStrLn (show ply ++ " wins!") else playGame newGm (switchPlayer ply)
        playGame newGm (switchPlayer ply)

-- main :: IO () -- main that asks for number of rows and columns
-- main = do
--     putStrLn "Enter the number of rows"
--     rows <- getLine
--     putStrLn "Enter the number of columns"
--     cols <- getLine
--     let gm = makeGame (read cols) (read rows)
--     playGame gm Red

main :: IO () -- main that uses constant number of rows and columns 
main = playGame (makeGame 6 6) Red