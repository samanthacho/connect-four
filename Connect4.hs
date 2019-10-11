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

connect4 :: Game
connect4 player move (State board colPos)
    | isColFull move colPos = RetryTurn player (State board colPos)
    | isTie newColPos = EndOfGame 'T' (State newBoard newColPos)
    | isWin player (move, newColPos!!move) newBoard = EndOfGame player (State newBoard newColPos)
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
    (isWinVertically player (x2, y1) (x2, y3) board 0) ||
    (isWinHorizontally player (x1, y2) (x3, y2) board 0) ||
    (isWinTopLeftToBottomRight player (x1, y1) (x3, y3) board 0) ||
    (isWinBottomLeftToTopRight player (x1, y3) (x3, y1) board 0)
    where
        x1 = max (x-3) 0; x2 = x; x3 = min(x+3) 6
        y1 = max (y-3) 0; y2 = y; y3 = min(y+3) 5

isWinVertically :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinVertically player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 || y1 > y2 = False
    | board!!y1!!x1 == player = isWinVertically player (x1+1, y1) (x2, y2) board (acc+1)
    | otherwise = isWinVertically player (x1+1, y1) (x2, y2) board 0

isWinHorizontally :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinHorizontally player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 || y1 > y2 = False
    | board!!y1!!x1 == player = isWinHorizontally player (x1, y1+1) (x2, y2) board (acc+1)
    | otherwise = isWinHorizontally player (x1, y1+1) (x2, y2) board 0

isWinTopLeftToBottomRight :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinTopLeftToBottomRight player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 || y1 > y2 = False
    | board!!y1!!x1 == player = isWinTopLeftToBottomRight player (x1+1, y1+1) (x2, y2) board (acc+1)
    | otherwise = isWinTopLeftToBottomRight player (x1+1, y1+1) (x2, y2) board 0

isWinBottomLeftToTopRight :: Player -> BoardPos -> BoardPos -> Board -> Int -> Bool
isWinBottomLeftToTopRight player (x1, y1) (x2, y2) board acc
    | acc >= 4 = True
    | x1 > x2 || y1 > y2 = False
    | board!!y1!!x1 == player = isWinBottomLeftToTopRight player (x1+1, y1-1) (x2, y2) board (acc+1)
    | otherwise = isWinBottomLeftToTopRight player (x1+1, y1-1) (x2, y2) board 0

-- updateList i newVal lst replaces the element at index i of lst with newVal
updateList :: Int -> t -> [t] -> [t]
updateList _ _ [] = []
updateList i newVal (h:t)
   | i == 0 = newVal:t
   | otherwise = h:updateList (i-1) newVal t
