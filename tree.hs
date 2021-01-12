import Data.List
import Test.HUnit

data Tree a = Node { root :: a,
                     forest :: [Tree a] } deriving (Show)

data PlaceHolder = X | O | Empty
  deriving (Eq, Show)

type Board = [PlaceHolder]

-- This function finds the solution to a given puzzle of size s
solve :: [Int] -> Int -> Int -> [Int]
solve puzzle r c = let tree = maketree puzzle r c in
                     let solutions = traversetree tree in
                       findsolution solutions r c
  
findsolution :: [[Int]] -> Int -> Int -> [Int]
findsolution [] r c = []
findsolution (f:tail) r c = case issolved f r c of
                              True -> f
                              False -> findsolution tail r c

-- Traverse the search tree
traversetree :: Tree [Int] -> [[Int]]
traversetree (Node x []) = [x]
traversetree (Node x xs) = t xs
  where
    t ((Node x xs):tail) = traversetree (Node x xs) ++ t tail
    t [] = []

-- Construct the search tree
maketree :: [Int] -> Int -> Int -> Tree [Int]
maketree puzzle r c = let npuzzles = (makec puzzle [])
                      in (Node puzzle (makenodes npuzzles r c []))

makenodes :: [[Int]] -> Int -> Int -> [Tree [Int]] -> [Tree [Int]]
makenodes [] r c a = a
makenodes (p:tail) r c a = makenodes tail r c ((maketree p r c):a)

makec:: [Int] -> [Int] -> [[Int]]
makec [] rem = []
makec (h:puzzle) rem = case h of
                         0 -> makec puzzle (0:rem)
                         1 -> makec puzzle (1:rem)
                         2 -> [reverse (puzzle ++ (0:rem)),
                               reverse (puzzle ++ (1:rem))] ++
                              makec puzzle (2:rem)   

-- Returns true if a puzzle is solved: It obeys the three rules of the game.
issolved :: [Int] -> Int -> Int -> Bool
issolved puzzle r c = let hslices = splitEvery c puzzle
                          vslices = transpose (splitEvery c puzzle)
                      in hastriples hslices vslices

splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- TEST CASES --
test_issolved1 = TestCase (assertEqual "for issolved [1,1,0,0] 2 2" False     (issolved [1,1,0,0] 2 2))
test_issolved2 = TestCase (assertEqual "for issolved [1,0,0,0] 2 2" False     (issolved [1,0,0,0] 2 2))
test_issolved3 = TestCase (assertEqual "for issolved [1,0,0,1] 2 2" True      (issolved [1,0,0,1] 2 2))
test_issolved4 = TestCase (assertEqual "for issolved [1] 1 1"       False     (issolved [1] 1 1))

tests_issolved = TestList [TestLabel "test1" test_issolved1,
                           TestLabel "test2" test_issolved2,
                           TestLabel "test3" test_issolved3,
                           TestLabel "test4" test_issolved4]

-- Returns true if the slices have triples
hastriples :: [[Int]] -> [[Int]] -> Bool
hastriples hslices vslices = if any (checkfortriples) hslices || any (checkfortriples) vslices
                             then False
                             else hasimbalance hslices vslices

-- Returns true if the numbers of 0 and 1 is different in a row or columns
hasimbalance :: [[Int]] -> [[Int]] -> Bool
hasimbalance hslices vslices = any (checkforimbalance) hslices || any (checkforimbalance) vslices
                               then False
                               else True

-- Returns true if a row is a copy of another, or a column is a copy of another
hasduplicates :: [[Int]] -> [[Int]] -> Bool
hasduplicates hslices vslices = length hslices /= length (nub hslices) ||
                                length vslices /= length (nub vslices)

-- TEST CASES --
test_hasduplicates1 = TestCase (assertEqual "for hasduplicates [[1,1],[0,0]] [[1,0],[1,0]]" True  (hasduplicates [[1,1],[0,0]] [[1,0],[1,0]]   ))
test_hasduplicates2 = TestCase (assertEqual "for hasduplicates [[0,1],[1,0]] [[0,0],[1,1]]" False (hasduplicates [[0,1],[1,0]] [[0,0],[1,1]]   ))

tests_hasduplicates = TestList [TestLabel "test1" test_hasduplicates1,
                                TestLabel "test2" test_hasduplicates2]

checkfortriples :: [Int] -> Bool
checkfortriples (x:y:z:tail) = if x==y && y==z
                               then True
                               else checkfortriples (y:z:tail)
checkfortriples l = False

checkforimbalance :: [Int] -> Bool
checkforimbalance l = length (filter (==0) l) /= length (filter (==1) l) 
