module Tests where

import Connect4
import IO
import Solver



testBM :: Int -> GameState
testBM x
    | x == 1 = gm
    | x == 2 = answer
    | x == 3 = gm2
    | x == 4 = answer2
    | x == 5 = gm3
    | x == 6 = answer3
    | x == 7 = gm4
  where
    gm = (Red,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Nothing, Nothing, Nothing,Nothing],
              [Just Red, Just Red, Just Red, Nothing, Nothing, Nothing, Nothing]])
    answer = (Black,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Nothing, Nothing, Nothing,Nothing],
              [Just Red, Just Red, Just Red, Just Red, Nothing, Nothing,Nothing]])
    gm2 = (Red,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Nothing, Nothing, Nothing,Nothing],
              [Just Red, Just Red, Just Red, Just Black, Just Red, Just Red,Just Red]])
    answer2 = (Black,
           rotateGame
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
              [Just Black, Just Black, Just Black, Just Black, Nothing, Nothing, Nothing],
              [Just Red, Just Red, Just Red, Just Black, Just Red, Just Red, Just Red]])
    gm3 = (Red,
             [[Nothing, Nothing, Nothing, Nothing, Nothing, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Black, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Just Black],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]])
    answer3 = (Black,
           [[Nothing, Nothing, Nothing, Nothing, Nothing, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Red, Just Black],
                [Nothing, Nothing, Just Red, Just Black, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Just Black],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]])
    gm4 = (Black, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
            [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
            [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
            [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
            [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
            [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
            [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])

bestMoveTest :: GameState -> GameState -> IO()
bestMoveTest gmSt answer =
    if answer == moveMade then putStrLn "Test Passed"
    else do putStrLn "Test Failed"
            putStrLn ("Expected: \n" ++ prettyPrintGame (snd answer))
            putStrLn ("But got: \n" ++ prettyPrintGame (snd moveMade))
    where gmAnswer = snd answer
          moveMade = makeMove (bestMove gmSt) gmSt

multiWinTest :: GameState -> IO()
multiWinTest gmSt =
  if answer1 == moveMade || answer2 == moveMade || answer3 == moveMade || answer4 == moveMade then putStrLn "Test Passed"
  else do putStrLn "Test Failed"
          putStrLn ("Expected: \n" ++ prettyPrintGame (snd answer1))
          putStrLn ("Or: \n"++ prettyPrintGame (snd answer2))
          putStrLn ("Or: \n"++ prettyPrintGame (snd answer3))
          putStrLn ("But got: \n" ++ prettyPrintGame (snd moveMade))

  where moveMade = makeMove (bestMove gmSt) gmSt
        answer1 = (Red, [[Nothing, Nothing, Just Black, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])
        answer2 = (Red, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Just Black, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])
        answer3 = (Red, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Just Black, Just Red, Just Black, Just Red]])
        answer4 = (Red, [[Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Just Black, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red],
                [Nothing, Nothing, Nothing, Just Black, Just Red, Just Black],
                [Nothing, Nothing, Nothing, Just Red, Just Black, Just Red]])
-- bestMoveTest (testBM 1) (testBM 2) -- test for bestMove
runTests :: IO ()
runTests = do
    putStrLn "\nTest 1\n"
    bestMoveTest (testBM 1) (testBM 2)
    putStrLn "\nTest 2\n"
    bestMoveTest (testBM 3) (testBM 4)
    putStrLn "\nTest 3\n"
    bestMoveTest (testBM 5) (testBM 6)
    putStrLn "\nTest 4\n"
    multiWinTest (testBM 7)

