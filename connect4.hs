

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

findLastEmpty :: [Player] -> Int --finds the last empty spot in a column
findLastEmpty col = if elem == Empty then length col else findLastEmpty (init col)
    where elem = last col
changeLastEmpty :: [Player] -> Player -> [Player] -- changes the last empty spot in a column to a player
changeLastEmpty col ply = if last col == Empty then (init col) ++ [ply] else changeLastEmpty (init col) ply -- potential error

cLE2 :: [Player] -> [Player] -> Player -> [Player] -- changes the last empty spot in a column to a player
cLE2 col bh ply = if last col == Empty then (init col) ++ [ply] ++ bh else cLE2 (init col) ([last col]++ bh) ply -- potential error
        

playerToChar :: Player -> Char -- converts player to char
playerToChar Red = 'R'
playerToChar Black = 'B'
playerToChar Empty = '.'

findColNum :: Game -> Game -> Int -> [Player] -- finds the nth column in a game
findColNum gm bkGm n = if n == 1 then head gm else findColNum (tail gm) bkGm (n-1)

findBackGame :: Game -> Game -> Int -> Game -- finds the nth column in a game
findBackGame gm bkGm n = if n == 1 then gm else findBackGame (tail gm) (([last gm]) ++ bkGm) (n-1)

mkMv :: Int -> Game -> Player -> Game -- makes a move in a game
mkMv n gm ply = if n > length (head gm) || n < 1 then gm else function
    where function = (init gm) ++ [cLE2 (head bkGm) [] ply] ++ (tail bkGm)
          bkGm = findBackGame gm [] n

makeMove :: Int -> Game -> Player -> Game -- makes a move in a game
makeMove n gm ply = mkMv n gm ply

--makeMv2 :: Int -> Game -> Player -> Game -- makes a move in a game



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
    where nGm = gm

switchPlayer :: Player -> Player -- switches player
switchPlayer Red = Black
switchPlayer Black = Red

-- still working on checkStraightWin/Diag

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

main :: IO () -- main that asks for number of rows and columns
main = do
    putStrLn "Enter the number of rows"
    rows <- getLine
    putStrLn "Enter the number of columns"
    cols <- getLine
    let gm = makeGame (read rows) (read cols)
    playGame gm Red

-- main :: IO () -- main that uses constant number of rows and columns 
-- main = do
--     let rows = 6
--         cols = 7
--         gm = makeGame rows cols
--     playGame gm Red


