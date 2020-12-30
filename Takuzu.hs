module Takuzu where

data PlaceHolder = X | O | Empty 
    deriving (Eq,Show) 

-- O O Empty -> O O X
-- X X Empty -> X X O

{-nextMove :: PlaceHolder -> PlaceHolder
  nextMove X = O
  nextMove O = X
  nextMove Empty = Empty
-}
if NoneEmpty
ProgramExe--[Majorbasic1, Majorbasic2, Majorbasic3]

-- idea is to call this function for every row and column
basic1 :: [PlaceHolder] -> [PlaceHolder] -> [PlaceHolder]
basic1 acc [] = acc  
basic1 acc (x:y:z:xs) = 
    if (x == y) && (x /= Empty) && (z == Empty)
        then basic1 (x:acc) (y:nextMove x:xs)
        else basic1 (x:acc) (y:z:xs)
basic1 acc (z:xs) = basic1 (z:acc) xs

{-renderBoard :: [PlaceHolder] -> IO ()
renderBoard board = do
    putStrLn $ renderRow firstRow
    putStrLn $ renderRow secondRow
    putStrLn $ renderRow thirdRow
    where firstRow  = take n board
        secondRow = drop n . take n*2 $ board
        thirdRow  = drop n*3 board-}    