module Main where

import Connect4
import System.IO
import Data.Char

-- Players
p1 = 'X'
p2 = 'O'

-- play isOver player state runs the main loop of the game
play :: Bool -> Player -> State -> IO ()
play isOver player (State board colPos) =
    do
        if isOver
            then
                return ()
            else do 
                result <- playTurn player (State board colPos)
                (over, retry, (State newBoard newColPos)) <- endTurn result
                if retry 
                    then play over player (State newBoard newColPos)
                    else play over (if player == p1 then p2 else p1) (State newBoard newColPos)

-- playTurn player state plays a turn and returns the result of the turn
playTurn :: Player -> State -> IO Result
playTurn player (State board colPos) =
    do 
        printBoard board
        putStrLn "\n" -- extra empty line for sexiness
        putStrLn ("Player " ++ [player] ++ "\'s turn. Pick a column from 1-7.")
        input <- parseInput
        let x = input - 1
        return (connect4 player x (State board colPos))
                

-- endTurn result ends the turn and returns a bool to indicate if game is over, 
-- a bool to indicate whether to retry the turn, and the game state
endTurn :: Result -> IO (Bool, Bool, State)
endTurn (EndOfGame player (State board colPos)) =
    do 
        if player == 'T' 
            then do 
                putStrLn "Tie! Game over."
                printBoard board
                return (True, False, (State board colPos))
            else do
                putStrLn ("Player " ++ [player] ++ " won! Game over.")
                printBoard board
                return (True, False, (State board colPos))

endTurn (ContinueGame (State board colPos)) =
    do
        return (False, False, (State board colPos))

endTurn (RetryTurn player (State board colPos)) =
    do
        putStrLn "That column is full. Try another."
        return (False, True, (State board colPos))

-- parseInput parses the user input for the game
parseInput :: IO Int
parseInput = 
    do 
        c <- getChar
        putStrLn "\n" -- extra empty line for sexiness
        if isValidGameInput c
            then 
                return (digitToInt c)
            else do 
                putStrLn "Invalid move please try again."
                parseInput

-- isValidGameInput c checks if c is a valid game input
isValidGameInput :: Char -> Bool
isValidGameInput c = c `elem` ['1', '2', '3', '4', '5', '6', '7']

-- printBoard b prints board b to console
printBoard :: Show a => [[a]] -> IO ()
printBoard b = 
    do
        putStrLn "***************************"
        putStr (unlines [unwords [show (b !! y !! x) | x <- [0..6]] | y <- [0..5]])
        putStrLn "***************************"

-- main method to start game
main :: IO ()
main = 
    do
        putStrLn "Connect 4"
        putStrLn "Player 1 is 'X' and Player 2 is 'O'"
        putStrLn "\n" -- extra empty line for sexiness
        play False p1 initState
