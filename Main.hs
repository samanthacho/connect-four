module Main where

import Connect4
import System.IO
import Data.Char
import System.Random

-- Players
p1 = 'X'
p2 = 'O'
column_numbers = ['1', '2', '3', '4', '5', '6', '7']
column_int     = [0, 1, 2, 3, 4, 5, 6]

-- play isOver player state runs the main loop of the game
play :: Bool -> Bool -> Player -> State -> Int -> IO ()
play isAi isOver player (State board colPos lastMove) level =
    do
        if isOver
            then
                return ()
            else do
                result <- playTurn isAi player (State board colPos lastMove) level
                (over, retry, (State newBoard newColPos lastMove)) <- endTurn result
                if retry
                    then play isAi over player (State newBoard newColPos lastMove) level
                    else play isAi over (if player == p1 then p2 else p1) (State newBoard newColPos lastMove) level

-- playTurn player state plays a turn and returns the result of the turn
playTurn :: Bool -> Player -> State -> Int -> IO Result
playTurn isAi player (State board colPos lastMove) level =
    do
        if (isAi && player == p1)
            then do
                putStrLn "Computer's turn."
                putStrLn "\n" -- extra empty line for sexiness
                if (level == 0 || level == 1) then do
                    input <- createAiMove level lastMove
                    let x = purifierFunction input
                    return (connect4 player x (State board colPos lastMove))
                else do
                    op_move <- moveChecker column_int player (State board colPos lastMove)
                    if (op_move == 0) then do
                        input2 <- createAiMove level lastMove
                        let x1 = purifierFunction input2
                        return (connect4 player x1 (State board colPos lastMove))
                    else do
                        let checker = purifierFunction op_move
                        return (connect4 player checker (State board colPos lastMove))
            else do
                printBoard board
                putStrLn "\n" -- extra empty line for sexiness
                putStrLn ("Player " ++ [player] ++ "\'s turn. Pick a column from 1-7.")
                input <- parseInput
                let z = input - 1
                return (connect4 player z (State board colPos lastMove))

atRandIndex :: [a] -> IO a
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i

-- endTurn result ends the turn and returns a bool to indicate if game is over,
-- a bool to indicate whether to retry the turn, and the game state
endTurn :: Result -> IO (Bool, Bool, State)
endTurn (EndOfGame player (State board colPos lastMove)) =
    do
        if player == 'T'
            then do
                putStrLn "Tie! Game over."
                printBoard board
                return (True, False, (State board colPos lastMove))
            else do
                putStrLn ("Player " ++ [player] ++ " won! Game over.")
                printBoard board
                return (True, False, (State board colPos lastMove))

endTurn (ContinueGame (State board colPos lastMove)) =
    do
        return (False, False, (State board colPos lastMove))

endTurn (RetryTurn player (State board colPos lastMove)) =
    do
        putStrLn "That column is full. Try another."
        return (False, True, (State board colPos lastMove))

createAiMove :: Int -> Int -> IO Int
createAiMove level lastMove =
    do
        if level == 0 then do
            x <- atRandIndex column_numbers
            if isValidGameInput x
                then
                    return (digitToInt x)
                else do createAiMove level lastMove
        else do
            x <- atRandIndex [intToDigit lastMove, intToDigit (lastMove + 1), intToDigit (lastMove + 2)]
            if isValidGameInput x
                then
                    return (digitToInt x)
                else do createAiMove 0 lastMove

moveChecker :: [Int] -> Player -> State -> IO Int
moveChecker [] _ (State board colPos lastMove) =
    do
        return 0
moveChecker (h:t) player (State board colPos lastMove) =
    do
        -- check CPU win
        if (colPos!!h >= 0) then do
            let (State newBoard newColPos lastMove) = updateBoard p1 h (State board colPos lastMove)
            if isWin player (h, ((newColPos!!h) + 1)) newBoard
                then do
                    return (h + 1)
                else do
                    -- check player win
                    let (State newBoard newColPos lastMove) = updateBoard p2 h (State board colPos lastMove)
                    if isWin (if player == p1 then p2 else p1) (h, ((newColPos!!h) + 1)) newBoard
                        then do
                            return (h+1)
                        else do
                            moveChecker t player (State board colPos lastMove)
            else do
                moveChecker t player (State board colPos lastMove)

purifierFunction :: Int -> Int
purifierFunction x = x - 1

zeroPurifier :: Int -> Int
zeroPurifier x = x + 0

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
isValidGameInput c = c `elem` column_numbers

-- printBoard b prints board b to console
printBoard :: Show a => [[a]] -> IO ()
printBoard b =
    do
        putStrLn "***************************"
        putStr (unlines [unwords [show (b !! y !! x) | x <- [0..6]] | y <- [0..5]])
        putStrLn "***************************"

-- isValidLevel l checks to see if it is a valid input
isValidLevel :: Char -> Bool
isValidLevel c = c `elem` ['1', '0', '2']

-- main method to start game
main :: IO ()
main =
    do
        putStrLn "Connect 4"
        putStrLn "Press c for single player, any other key for multi player"
        c <- getChar
        if (c == 'c')
            then do
                putStrLn "\n" -- extra empty line for sexiness
                putStrLn "Computer is player 'X'. You are Player 0."
                putStrLn "Choose either 0, 1, or 2 for difficulty"
                l <- getChar
                if isValidLevel l
                    then do
                        putStrLn "\n" -- extra empty line for sexiness
                        play True False p1 initState (digitToInt l)
                    else do
                        putStrLn "\n" -- extra empty line for sexiness
                        putStrLn "Not a valid level input. Redirecting to main."
                        main
        else do
            putStrLn "Player 1 is 'X' and Player 2 is 'O'"
            putStrLn "\n" -- extra empty line for sexiness
            play False False p1 initState 0
