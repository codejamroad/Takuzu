data Tree a = Node { root :: a,
                     forest :: [Tree a] } deriving (Show)

-- This type is currently not used
data Puzzle = Puzzle [Int] Int deriving (Show)

-- This function finds the solution to a given puzzle of size s
solve :: [Int] -> Int -> [Int]
solve puzzle s = let tree = maketree puzzle s in
                   let solutions = traversetree tree in
                     findsolution solutions s
  
findsolution :: [[Int]] -> Int -> [Int]
findsolution [] s = []
findsolution (f:tail) s = case checkpuzzle f s of
                            True -> f
                            False -> findsolution tail s

-- Traverse the search tree
traversetree :: Tree [Int] -> [[Int]]
traversetree (Node x []) = [x]
traversetree (Node x xs) = t xs
  where
    t ((Node x xs):tail) = traversetree (Node x xs) ++ t tail
    t [] = []

-- Construct the search tree
maketree :: [Int] -> Int -> Tree [Int]
maketree puzzle s = let npuzzles = (makec puzzle [])
                    in (Node puzzle (makenodes npuzzles s []))

makenodes :: [[Int]] -> Int -> [Tree [Int]] -> [Tree [Int]]
makenodes [] s a = a
makenodes (p:tail) s a = makenodes tail s ((maketree p s):a)

makec:: [Int] -> [Int] -> [[Int]]
makec [] rem = []
makec (h:puzzle) rem = case h of
                         0 -> makec puzzle (0:rem)
                         1 -> makec puzzle (1:rem)
                         2 -> [reverse (puzzle ++ (0:rem)),
                               reverse (puzzle ++ (1:rem))] ++
                              makec puzzle (2:rem)   


-- Check if a puzzle is solved or not: This is temporary and inaccurate.
-- This implementation calls a puzzle solved if that puzzle has no unassigned values 
checkpuzzle :: [Int] -> Int -> Bool
checkpuzzle puzzle s = if length ([x | x <- puzzle, x==2]) == 0
                       then True
                       else False
