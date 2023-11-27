module Win where

import Connect4

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

winnerOfGame :: GameState -> Maybe Winner -- returns the winner of a game
winnerOfGame (ply, game)
  | checkWin game (Just Red) = Just (Win Red) -- Red Win
  | checkWin game (Just Black) = Just (Win Black) -- Black Win
  | null moves = Just Tie -- Tie
  | otherwise = Nothing -- Game is ongoing
  where moves = getAvailableMoves (ply, game)

-- Game play logic

getAvailableMoves :: GameState -> [Move] -- gets the available moves in a game
getAvailableMoves (ply, gm) = [x | x <- [1..length gm], legalMove gm x]


-- printing logic


