

--data Board =  -- board size
data Player = Red | Black | Empty deriving (Show, Eq)
type Game = [[Player]]
type Winner = Player
-- stuff the needs to be done
-- make game contain current player
-- add current player to makeMove
-- check legal moves

-- Define data types or type aliases for a player, game state, move, and winner.

-- Be able to determine who has won the game state, if anyone.

-- Be able to compute the result of making a legal move in a game state.

-- Be able to compute the legal moves from a game state.

-- (If time) Be able to pretty-print a game into a string.

testGame :: Game -- test game
testGame = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Black,Empty,Empty,Empty],
            [Empty,Empty,Empty,Red,Empty,Empty,Empty],
            [Empty,Empty,Empty,Black,Empty,Empty,Empty],
            [Empty,Empty,Empty,Red,Empty,Empty,Empty]]

testGame2 :: Game -- test game with a win
testGame2 = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Black,Empty,Empty,Empty],
            [Empty,Empty,Empty,Red,Black,Empty,Empty],
            [Empty,Empty,Empty,Black,Black,Black,Empty],
            [Empty,Empty,Empty,Red,Red,Red,Black]]


makeGame :: Int -> Int -> Game -- makes a blank game of size n x m
makeGame n m = [[Empty | x <- [1..n]] | y <- [1..m]]

playerToChar :: Player -> Char -- converts player to char
playerToChar Red = 'R'
playerToChar Black = 'B'
playerToChar Empty = '.'

-- move functions

cLE :: [Player] -> Player -> [Player] -> [Player] -- changes the last empty spot in a column to a player
cLE col ply bh = 
    if last col == Empty 
        then (init col) ++ [ply] ++ bh 
        else cLE (init col) ply ([last col] ++ bh)

backGame :: Game -> Int -> Game -- finds the nth column in a game
backGame gm n = if n == 1 then gm else backGame (tail gm) (n-1)

frontGame :: Game -> Int -> Game -- finds the nth column in a game
frontGame gm n = if n == length gm then init gm else frontGame (init gm) (n)

canMkMv :: Int -> Game -> Bool -- checks if a move can be made in a game
canMkMv n gm = if n > length (head gm) || n < 1 then False else function
    where function = if last (findColNum gm [] n) == Empty then True else False

mkMv :: Int -> Game -> Player -> Game -- makes a move in a game
mkMv n gm ply = frntGm ++ [cLE (head bkGm) ply []] ++ (tail bkGm)
    where bkGm = backGame gm n
          frntGm = frontGame gm n

makeMove :: Int -> Game -> Player -> Game -- makes a move in a game
makeMove n gm ply = if canMkMv n gm then mkMv n gm ply else gm

-- orentation functions

rotateGame :: Game -> Game -- rotates a game 90 degrees
rotateGame gm = if length (head gm) == 0 then [] else (map last gm) : rotateGame (map init gm)

rotateGame2 :: Game -> Game -- rotates a game 90 degrees
rotateGame2 gm = if length (last gm) == 0 then [] else (map head gm) : rotateGame2 (map tail gm)

flipHori :: Game -> Game -- flips a game horizontally
flipHori gm = if length gm == 0 then [] else (reverse (head gm)) : flipHori (tail gm)

gameToString :: Game -> String -- converts a game to a string
gameToString gm = if length gm == 0 then [] else function
    where function = (map playerToChar (head gm)) ++ "\n" ++ gameToString (tail gm)

displayGame :: Game -> IO () -- displays a game
displayGame gm = putStrLn (gameToString (rotateGame2 nGm))
--displayGame gm = putStrLn (gameToString (nGm))
    where nGm = gm

--player logic

switchPlayer :: Player -> Player -- switches player
switchPlayer Red = Black
switchPlayer Black = Red

-- game logic

chkStrW :: Game -> Player -> Bool -- checks if a player has won in a straight line
chkStrW gm ply = if length gm < 4 then False else function
    where function = chkStrW (tail gm) ply || chkStrW (init gm) ply || chkStrW (tail gm) ply || chkStrW (init gm) ply

checkWinDiag :: Game -> Player -> Bool -- checks if a player has won diagonally
checkWinDiag gm ply = undefined

checkWin :: Game -> Player -> Bool -- checks if a player has won
checkWin gm ply = chkStrW (rotateGame gm) ply || chkStrW gm ply || checkWinDiag gm ply

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

