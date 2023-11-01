import Data.ByteString (find)
import Control.Concurrent.STM (check)


--data Board =  -- board size
data Player = Red | Black deriving (Show, Eq)
type Game =  [[Int]]


    -- needed fuctions 
    -- make game - input board size
    -- make move - input int

testGame :: Game
testGame = [[0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0],
            [0,0,0,2,0,0,0],
            [0,0,0,1,0,0,0],
            [0,0,0,2,0,0,0],
            [0,0,0,1,0,0,0]]
testGame2 :: Game
testGame2 = [[0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0],
            [0,0,0,2,0,0,0],
            [0,0,0,1,2,0,0],
            [0,0,0,2,2,2,0],
            [0,0,0,1,1,1,2]]


makeGame :: Int -> Int -> Game
makeGame n m = [[0 | x <- [1..n]] | y <- [1..m]]

getElemAtDex :: [Int] -> Int -> Int
getElemAtDex row 1 = head row
getElemAtDex row n = getElemAtDex (tail row) (n-1)

checkIfEmptyInRow :: [Int] -> Int -> Bool
checkIfEmptyInRow row n = getElemAtDex row n == 0

getDeepestEmpty :: Game -> Int -> Int -> Int
getDeepestEmpty gm n depth = if elem == 0 then depth else getDeepestEmpty (init gm) n (depth-1)
    where elem = getElemAtDex (last gm) n

changeDeepestEmpty :: Game -> Int -> Int -> Int -> Game
changeDeepestEmpty gm n depth ply = if depth == 0 then gm else changeDeepestEmpty (init gm) n (depth-1) ply

makeMove :: Int -> Game -> Player -> Game --this does not work as expected
makeMove n gm ply = if n > length (head gm) || n < 1 then gm else function
    where playerToInt Red = 1
          playerToInt Black = 2
          function = changeDeepestEmpty gm n (getDeepestEmpty gm n (length gm)) (playerToInt ply)
  

    
gameToString :: Game -> String
gameToString gm = unlines (map (map intToChar) gm)
    where intToChar 0 = '.'
          intToChar 1 = 'R'
          intToChar 2 = 'B'

displayGame :: Game -> IO ()
displayGame gm = putStrLn (gameToString gm)

switchPlayer :: Player -> Player
switchPlayer Red = Black
switchPlayer Black = Red

-- still working on checkWinRow/Col/Diag

checkWinRow :: Game -> Player -> Bool
checkWinRow gm ply = any (== True) (map (checkWinRow' ply) gm)
    where checkWinRow' ply row = any (== True) (map (checkWinRow'' ply row) [1..length row])
          checkWinRow'' ply row n = getElemAtDex row n == playerToInt ply && getElemAtDex row (n+1) == playerToInt ply && getElemAtDex row (n+2) == playerToInt ply && getElemAtDex row (n+3) == playerToInt ply
          playerToInt Red = 1
          playerToInt Black = 2
checkWinCol :: Game -> Player -> Bool
checkWinCol gm ply = any (== True) (map (checkWinCol' ply) gm)
    where checkWinCol' ply col = any (== True) (map (checkWinCol'' ply col) [1..length col])
          checkWinCol'' ply col n = getElemAtDex col n == playerToInt ply && getElemAtDex col (n+1) == playerToInt ply && getElemAtDex col (n+2) == playerToInt ply && getElemAtDex col (n+3) == playerToInt ply
          playerToInt Red = 1
          playerToInt Black = 2
checkWinDiag :: Game -> Player -> Bool
checkWinDiag gm ply = checkWinDiag' gm ply || checkWinDiag' (reverse gm) ply
    where checkWinDiag' gm ply = any (== True) (map (checkWinDiag'' ply) gm)
          checkWinDiag'' ply row = any (== True) (map (checkWinDiag''' ply row) [1..length row])
          checkWinDiag''' ply row n = getElemAtDex row n == playerToInt ply && getElemAtDex (tail row) (n+1) == playerToInt ply && getElemAtDex (tail (tail row)) (n+2) == playerToInt ply && getElemAtDex (tail (tail (tail row))) (n+3) == playerToInt ply
          playerToInt Red = 1
          playerToInt Black = 2

checkWin :: Game -> Player -> Bool
checkWin gm ply = checkWinRow gm ply || checkWinCol gm ply || checkWinDiag gm ply

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


