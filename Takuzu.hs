module Takuzu where

data PlaceHolder = X | O | Empty
  deriving (Eq, Show)

type Board = [PlaceHolder]

nextMove :: PlaceHolder -> PlaceHolder
nextMove O = X
nextMove Empty = Empty
nextMove X = O


-- Basic Techniques
-- O O Empty -> O O X
-- X X Empty -> X X O
-- idea is to call this function for every row and column
basic1 :: Board -> Board -> Board
basic1 acc [] = acc
basic1 acc (x : y : z : xs) =
  if (x == y) && (x /= Empty) && (z == Empty)
    then basic1 (x : acc) (y : nextMove x : xs)
    else basic1 (x : acc) (y : z : xs)
basic1 acc (z : xs) = basic1 (z : acc) xs

majorBasic1 :: Board -> Board
majorBasic1 = basic1 []

majorBasic2 :: Board -> Board
majorBasic2 = basic1 []

{-renderBoard :: Board -> IO ()
renderBoard board = do
    putStrLn $ renderRow firstRow
    putStrLn $ renderRow secondRow
    putStrLn $ renderRow thirdRow
    where firstRow  = take n board
        secondRow = drop n . take n*2 $ board
        thirdRow  = drop n*3 board-}

ruleList :: [Board -> Board]
ruleList =
  [majorBasic1, majorBasic2]

--- countEmptyCells
isEmpty :: [PlaceHolder] -> Bool
isEmpty [] = False
isEmpty (x : xs)
  | x == Empty = True
  | otherwise = isEmpty xs

runRule :: Board -> (Board -> Board) -> Board
runRule board ruleFn =
  let newBoard = ruleFn board
   in if board == newBoard
        then board
        else runRule newBoard ruleFn

tryAllRules :: Board -> [Board -> Board] -> Board
tryAllRules board [] = board
tryAllRules board (rule : rules) =
  let newBoard = runRule board rule
   in if isEmpty newBoard
        then newBoard
        else tryAllRules newBoard rules

solve :: Board -> [Board -> Board] -> Board
solve board allRule =
  let newBoard = tryAllRules board allRule
   in if newBoard == board
        then newBoard
        else tryAllRules newBoard allRule

readLines :: Int -> Int -> Board -> IO Board
readLines _ 0 board = return (reverse board)
readLines size rowsLeft board = do
    row <- getLine 
    let rowList = row
    if length rowList == size
        then readLines size (rowsLeft - 1) (rowList:board)
        else printError "Invalid row length"

main :: IO ()
main = do
    size <- getLine
    readLines size size []
