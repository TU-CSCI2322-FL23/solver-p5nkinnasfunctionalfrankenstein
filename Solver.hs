module Solver where

import Connect4
import Win
import Data.Maybe
import IO

type Rating = Int


-- HELPER FUNCTION FOR bestMove
evaluateBoard :: GameState -> Int
evaluateBoard gameState@(player, game)
  | checkWin game (Just Red) = if player == Red then 1000 else -1000
  | checkWin game (Just Black) = if player == Black then 1000 else -1000
  | otherwise = 0


-- HELPER FUNCTION FOR bestMove
minimax :: GameState -> Int -> Bool -> Int
minimax gameState@(player, game) depth isMaximizingPlayer
  | depth == 0 || isGameDecided (winnerOfGame gameState) = evaluateBoard gameState
  | isMaximizingPlayer =
      maximum [ minimax (makeMove move gameState) (depth - 1) False | move <- getAvailableMoves gameState, legalMove game move ]
  | otherwise =
      minimum [ minimax (makeMove move gameState) (depth - 1) True | move <- getAvailableMoves gameState, legalMove game move ]
  where
    isGameDecided :: Maybe Winner -> Bool
    isGameDecided Nothing = False
    isGameDecided (Just _) = True

bestMove :: GameState -> Move
bestMove gameState@(player, game) =
  let initPlayer = player
      moves = getAvailableMoves gameState
      moveScores = [(minimax (makeMove move gameState) depth (player /= Red), move) | move <- moves]
      bestScore
        | player == Red = maximum moveScores
        | otherwise = minimum moveScores
      depth = 5
  in snd bestScore


whoWillWin :: GameState -> Maybe Winner -- checks who will win -- doesn't account for ties
whoWillWin gmSt =
  if isNothing (winnerOfGame gmSt)
  then whoWillWin newGmSt
  else winnerOfGame gmSt
  where ply = fst gmSt
        gm = snd gmSt
        moves = getAvailableMoves gmSt
        newGmSt = makeMove (bestMove gmSt) gmSt


whoWillWin2 :: GameState -> Maybe Winner -- checks who will win -- doesn't account for ties
whoWillWin2 gmSt = undefined


putBestMove :: GameState -> IO () -- puts the best move in a game -- not done
putBestMove gmSt = do
    let ply = fst gmSt
        gm = snd gmSt
        moves = getAvailableMoves gmSt
        bestMove = head moves
    putStrLn ("The best move is " ++ show bestMove)
    playGame (makeMove bestMove gmSt)


rateGame :: Game -> Rating
rateGame game
  | checkWin game (Just Red) = 1000  -- Best state for Red
  | checkWin game (Just Black) = -1000 -- Worst state for Red 
  | otherwise = sum $ map rateColumn game
  where
    rateColumn :: Column -> Rating
    rateColumn col = let
        redCount = length $ filter (== Just Red) col
        blackCount = length $ filter (== Just Black) col
      in redCount - blackCount  -- Positive if favorable for Red, negative for Black





-- checkWinWithK :: Int -> GameState -> Bool
-- checkWinWithK k (ply, gm) = any (winWithK ply ((diagonals1 gm) ++ (diagonals2 gm))) k || any (winWithK ply gm) k

-- checkForK :: Int -> GameState -> Move -> Move
-- checkForK _ _ 0 = -1
-- checkForK k (ply, gm) mv = if checkWinWithK k (makeMove mv (ply, gm)) then mv else checkForK k (ply, gm) (mv+1)

-- itK :: Int -> GameState -> Move
-- itK 0 _ = -1
-- itK k (ply, gm) = if rowRes == -1 then itK (k-1) (ply, gm) else rowRes
--   where rowRes = checkForK k (ply, gm) 1

-- bestMove2 :: GameState -> Move 
-- bestMove2 gmSt = if res == -1 then 7 else res 
--   where k = 4
--         res = itK k gmSt 
--         --moves = getAvailableMoves gameState 

