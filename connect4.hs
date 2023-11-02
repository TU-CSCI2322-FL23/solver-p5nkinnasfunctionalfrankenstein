

--data Board =  -- board size
data Player = Red | Black | Empty deriving (Show, Eq)
type Game = [[Player]]


testGame :: Game
testGame = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Black,Empty,Empty,Empty],
            [Empty,Empty,Empty,Red,Empty,Empty,Empty],
            [Empty,Empty,Empty,Black,Empty,Empty,Empty],
            [Empty,Empty,Empty,Red,Empty,Empty,Empty]]

testGame2 :: Game
testGame2 = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Empty,Empty,Empty,Empty],
            [Empty,Empty,Empty,Black,Empty,Empty,Empty],
            [Empty,Empty,Empty,Red,Black,Empty,Empty],
            [Empty,Empty,Empty,Black,Black,Black,Empty],
            [Empty,Empty,Empty,Red,Red,Red,Black]]


makeGame :: Int -> Int -> Game
makeGame n m = [[Empty | x <- [1..n]] | y <- [1..m]]

findLastEmpty :: [Player] -> Int
findLastEmpty col = if elem == Empty then length col else findLastEmpty (init col)
    where elem = last col
changeLastEmpty :: [Player] -> Player -> [Player]
changeLastEmpty col ply = if last col == Empty then (init col) ++ [ply] else changeLastEmpty (init col) ply

playerToChar :: Player -> Char
playerToChar Red = 'R'
playerToChar Black = 'B'

findColNum :: Game -> Int -> [Player]
findColNum gm n = if n == 1 then head gm else findColNum (tail gm) (n-1)

makeMove :: Int -> Game -> Player -> Game
makeMove n gm ply = if n > length (head gm) || n < 1 then gm else function
    where function = (init gm) ++ [changeLastEmpty (findColNum gm n) ply]


rotateGame :: Game -> Game
rotateGame gm = if length (head gm) == 0 then [] else (map last gm) : rotateGame (map init gm)

displayGame :: Game -> IO ()
displayGame gm = rotateGame gm >>= putStrLn . map playerToChar

switchPlayer :: Player -> Player
switchPlayer Red = Black
switchPlayer Black = Red

-- still working on checkStraightWin/Diag

checkWin :: Game -> Player -> Bool
checkWin gm ply = chkStrW rotateGame (gm) ply || chkStrW gm ply || checkWinDiag gm ply

chkStrW :: Game -> Player -> Bool
chkStrW gm ply = if length gm < 4 then False else function
    where function = chkStrW (tail gm) ply || chkStrW (init gm) ply || chkStrW (tail gm) ply || chkStrW (init gm) ply

checkWinDiag :: Game -> Player -> Bool
checkWinDiag gm ply = undefined


playGame :: Game -> Player -> IO ()
playGame gm ply = do
    displayGame gm
    putStrLn "Enter a column number to make a move"
    col <- getLine
    let newGm = makeMove (read col) gm ply
    displayGame newGm
    if checkWin newGm ply then putStrLn (show ply ++ " wins!") else playGame newGm (switchPlayer ply)

main :: IO ()
main = do
    putStrLn "Enter the number of rows"
    rows <- getLine
    putStrLn "Enter the number of columns"
    cols <- getLine
    let gm = makeGame (read rows) (read cols)
    playGame gm Red

-- main that uses constant number of rows and columns 
-- main :: IO ()
-- main = do
--     let rows = 6
--         cols = 7
--         gm = makeGame rows cols
--     playGame gm Red


