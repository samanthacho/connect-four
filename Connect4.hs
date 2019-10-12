module Connect4 where

data State = State Board ColPos
            deriving (Ord, Eq, Show)

data Result = EndOfGame Player State
            | ContinueGame State
            | RetryTurn Player State
            deriving (Eq)

type Game = Player -> Col -> State -> Result

type Player = Char

type Col = Int

type BoardPos = (Int, Int)

type Board = [[Char]]

type ColPos = [Int]

-- connect4 plays a turn of the game and updates the game baord
connect4 :: Game
connect4 player move (State board colPos)
    | isColFull move colPos = RetryTurn player (State board colPos)
    | isTie newColPos = EndOfGame 'T' (State newBoard newColPos)
    | isWin player (move, colPos!!move) newBoard = EndOfGame player (State newBoard newColPos)
    | otherwise = ContinueGame (State newBoard newColPos)
    where
        (State newBoard newColPos) = updateBoard player move (State board colPos)

-- initState returns the initial State of the game
initState :: State
initState = State [['*', '*', '*', '*', '*', '*', '*'], 
                   ['*', '*', '*', '*', '*', '*', '*'],
                   ['*', '*', '*', '*', '*', '*', '*'],
                   ['*', '*', '*', '*', '*', '*', '*'],
                   ['*', '*', '*', '*', '*', '*', '*'],
                   ['*', '*', '*', '*', '*', '*', '*']] [5,5,5,5,5,5,5]

-- updateBoard player x state returns the new updated State of the game 
updateBoard :: Player -> Col -> State -> State
updateBoard player x (State board colPos) =
    let y = colPos!!x
        rowToUpdate = board!!y
        updatedColPos = updateList x (y-1) colPos
        updatedRow = updateList x player rowToUpdate
        updatedBoard = updateList y updatedRow board
    in (State updatedBoard updatedColPos)

isColFull :: Col -> ColPos -> Bool
isColFull move colPos = colPos!!move < 0

isTie :: ColPos -> Bool
isTie [] = True
isTie (h:r) = h < 0 && isTie r

isWin :: Player -> BoardPos -> Board -> Bool
isWin player (x, y) board = 
    (isWinVertically player (x, top) (x, bottom) board 0) ||
    (isWinHorizontally player (left, y) (right, y) board 0) ||
    (isWinTopLeftToBottomRight player (tlx, tly) (brx, bry) board 0) ||
    (isWinBottomLeftToTopRight player (blx, bly) (trx, try) board 0)
    where
        left = max (x-3) 0; right = min (x+3) 6
        top = max (y-3) 0; bottom = min (y+3) 5
        (tlx, tly) = findTopLeft (x, y) 3; (trx, try) = findTopRight (x, y) 3
        (blx, bly) = findBottomLeft (x, y) 3; (brx, bry) = findBottomRight (x, y) 3

isWinVertically :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinVertically player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | y1 > y2 = False
    | board!!y1!!x1 == player = isWinVertically player (x1, y1+1) (x2, y2) board (acc+1)
    | otherwise = isWinVertically player (x1, y1+1) (x2, y2) board 0

isWinHorizontally :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinHorizontally player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 = False
    | board!!y1!!x1 == player = isWinHorizontally player (x1+1, y1) (x2, y2) board (acc+1)
    | otherwise = isWinHorizontally player (x1+1, y1) (x2, y2) board 0

isWinTopLeftToBottomRight :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinTopLeftToBottomRight player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 || y1 > y2 = False
    | board!!y1!!x1 == player = isWinTopLeftToBottomRight player (x1+1, y1+1) (x2, y2) board (acc+1)
    | otherwise = isWinTopLeftToBottomRight player (x1+1, y1+1) (x2, y2) board 0

isWinBottomLeftToTopRight :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinBottomLeftToTopRight player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 || y1 < y2 = False
    | board!!y1!!x1 == player = isWinBottomLeftToTopRight player (x1+1, y1-1) (x2, y2) board (acc+1)
    | otherwise = isWinBottomLeftToTopRight player (x1+1, y1-1) (x2, y2) board 0

findTopLeft (x, y) dist
    | dist == 0 = (x, y)
    | x == 0 || y == 0 = (x, y)
    | otherwise = findTopLeft (x-1, y-1) (dist-1)

findTopRight (x, y) dist
    | dist == 0 = (x, y)
    | x == 6 || y == 0 = (x, y)
    | otherwise = findTopRight (x+1, y-1) (dist-1)

findBottomLeft (x, y) dist
    | dist == 0 = (x, y)
    | x == 0 || y == 5 = (x, y)
    | otherwise = findBottomLeft (x-1, y+1) (dist-1)

findBottomRight (x, y) dist
    | dist == 0 = (x, y)
    | x == 6 || y == 5 = (x, y)
    | otherwise = findBottomRight (x+1, y+1) (dist-1)

-- updateList i newVal lst replaces the element at index i of lst with newVal
updateList :: Int -> t -> [t] -> [t]
updateList _ _ [] = []
updateList i newVal (h:t)
   | i == 0 = newVal:t
   | otherwise = h:updateList (i-1) newVal t
