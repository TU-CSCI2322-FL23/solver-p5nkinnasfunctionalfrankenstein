import Data.ByteString (find)


--data Board =  -- board size
data Player = Red | Black
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

makeMove :: Int -> Game -> Player -> Game
makeMove n gm ply = changeDeepestEmpty gm n (getDeepestEmpty gm n (length gm)) (playerToInt ply)
    where playerToInt Red = 1
          playerToInt Black = 2
  

    
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

playGame :: Game -> Player -> IO ()
playGame gm ply = do
    displayGame gm
    putStrLn "Enter a column number to make a move"
    col <- getLine
    let newGm = makeMove (read col) gm ply
    displayGame newGm
    playGame newGm (switchPlayer ply)

main :: IO ()
main = do
    putStrLn "Enter the number of rows"
    rows <- getLine
    putStrLn "Enter the number of columns"
    cols <- getLine
    let gm = makeGame (read rows) (read cols)
    playGame gm Red





