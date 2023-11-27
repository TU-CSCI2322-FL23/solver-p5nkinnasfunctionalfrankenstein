module Connect4 where
import Data.Maybe
--import IO


data Player = Red | Black deriving (Show, Eq, Read)
type Column = [Maybe Player]
type Game = [Column]
data Winner = Win Player | Tie deriving (Show, Eq, Read)
type GameState = (Player, Game)
--type Winner = Maybe Player
type Move = Int

makeGameState :: Int -> Int -> GameState -- makes a blank game of size n x m
makeGameState n m = (Red, [[Nothing | x <- [1..n]] | y <- [1..m]])

-- move functions

dexColNum :: Game -> Move -> Column -- finds the nth column in a game
dexColNum gm 1 = head gm
dexColNum gm n = dexColNum (tail gm) (n-1)

colHasNothing :: Column -> Bool -- checks if a column has an Nothing spot
colHasNothing [] = False
colHasNothing (x:xs) = (x == Nothing) || colHasNothing xs

insertPlay :: Column -> Maybe Player -> Column -- changes the last Nothing spot in a column to a player
insertPlay (Nothing:rest) color = aux rest
   where aux (Nothing:rest) = Nothing:aux rest
         aux column = color:column

legalMove :: Game -> Move -> Bool -- checks if a move is legal
legalMove gm n =
    not (n > length gm || n < 1) && colHasNothing (dexColNum gm n)

makeMove :: Move -> GameState -> GameState -- makes a move in a game
makeMove mv gmSt = (switchPlayer ply, frntGm ++ [insertPlay (head bkGm) (Just ply)] ++ tail bkGm)
    where gm = snd gmSt
          ply = fst gmSt
          num = mv-1
          bkGm = drop num gm
          frntGm = take num gm

rotateGame :: Game -> Game -- rotates a game 90 degrees the other way
rotateGame gm = if length (last gm) == 0 then [] else map head gm : rotateGame (map tail gm)
--player logic

switchPlayer :: Player -> Player -- switches player
switchPlayer Red = Black
switchPlayer Black = Red

changePlayer :: GameState -> GameState -- changes the player in a game state
changePlayer (ply, gm) = (switchPlayer ply, gm)
