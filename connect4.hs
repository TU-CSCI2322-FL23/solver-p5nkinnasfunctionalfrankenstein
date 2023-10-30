

--data Board =  -- board size
data Player = Red | Black
type Game =  [[Int]]


    -- needed fuctions 
    -- make game - input board size
    -- make move - input int
makeGame :: Int -> Int -> Game
makeGame n m = [[0 | x <- [1..n]] | y <- [1..m]]

makeMove :: Int -> Game -> Game
makeMove n gm = undefined
    
gameToString :: Game -> String
gameToString gm = unlines (map (map intToChar) gm)
    where intToChar 0 = '.'
          intToChar 1 = 'R'
          intToChar 2 = 'B'

displayGame :: Game -> IO ()
displayGame gm = putStrLn (gameToString gm)

--makeGame :: Int -> Game
--makeGame n = [[0 | x <- [1..n]] | y <- [1..n]]
--





