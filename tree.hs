import Data.List
import Test.HUnit
import Data.Function (on)
import Test.QuickCheck
import Control.Monad
import Debug.Trace
--import Data.Universe.Helpers

-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

data Tree = Node { root :: Board,
                   solved :: Bool,
                   forest :: [Tree] } deriving (Show)

data Slot = X | O | E
  deriving (Eq, Show)

type Board = [Slot]
type Line = [Slot]
type NLine = (Int, Line)
type NSlot = (Int, Line)

------------------------
-- MAIN SOLVER AND IO --
------------------------

main :: IO ()
main = do
  input <- getContents
  let inp = lines input
      firstline = head inp
      puzzlestr = unlines (tail inp)
      puzzle = strtoline puzzlestr
      (r, c) = getdimensions firstline
      solution = solve puzzle r c
  putStr (puztoprintable solution r c)

getdimensions :: String -> (Int, Int)
getdimensions string = let inp = words string
                           r = head inp
                           c = head (tail inp)
                       in (read r :: Int, read c :: Int)

----------------------------
-- SOME UTILITY FUNCTIONS --
----------------------------

nextMove :: Slot -> Slot
nextMove O = X
nextMove X = O
nextMove E = E

swap :: Slot -> Slot -> Slot
swap a E = a
swap _ b = b

splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- (HUnit) Verifies that every non empty slot in a has the same value in b
-- That is: By assigning values to empty slots in a it can be made equal to b
canbemadeinto :: Board -> Board -> Bool
canbemadeinto a b = cmp a b
  where
    cmp (E:t1) (_:t2) = cmp t1 t2
    cmp (a:t1) (b:t2) = if a==b then cmp t1 t2 else False
    cmp [] []         = True
    cmp _ []          = False
    cmp [] _          = False

puztoprintable :: Board -> Int -> Int -> String
puztoprintable board r c = let lines = splitEvery c board
                           in concat (map (linetostr) lines)

--stringtopuzzle :: String -> Board
--stringtopuzzle p = map (strtoline)

linetostr :: Line -> String
linetostr (E:line) = '.' : linetostr line
linetostr (O:line) = 'O' : linetostr line
linetostr (X:line) = 'X' : linetostr line
linetostr [] = "\n" 

strtoline :: String -> Line
strtoline ('.':str) = E : strtoline str
strtoline ('O':str) = O : strtoline str
strtoline ('X':str) = X : strtoline str
strtoline (_:str) = strtoline str
strtoline [] = []

countSymbol :: Slot -> Line -> Int
countSymbol s line = w s line 0
  where
    w p [] acc = acc
    w p (h:line) acc = if p==h
                       then w p line (acc+1)
                       else w p line acc

-----------------------------------------
-- FUNCTIONS FOR CONSTRUCTING THE TREE --
-----------------------------------------

makenewboard :: Board -> Int -> Int -> Board
makenewboard puzzle r c = fix (applytechniques r c) puzzle

applytechniques :: Int -> Int -> Board -> Board
applytechniques r c puzzle = let row = splitEvery c puzzle
                                 out1 = transpose (map (basic3 . basic4 . basic12) row)
                                 out2 = transpose (map (basic3 . basic4 . basic12) out1)
                             in concat out2

--prop_split_inv c xs = unsplit c (split c xs) == xs
--prop_1 (NonEmpty xs) = forAll (choose (0, length xs - 1)) ( \i -> element_at xs i == (xs !! i :: Int))
--prop_bang x = x >= 0 ==> forAll (listLongerThan x) ( \xs -> element_at xs x == xs !! x )

--listLongerThan :: Int -> Gen [Slot]
--listLongerThan x = replicateM (x+1) arbitrary

-- NOT USED
applyonce :: (Line -> Line) -> Int -> Int -> [Line] -> [Line]
applyonce f r c slices = let nslices = transpose (map (f) slices)
                         in transpose (map (f) nslices)

-- The third parameter must be sorted!
scanlines :: (Line -> Line) -> [(Int, Line)] -> [Int] -> [(Int, Line)]
scanlines f ((l, line):lines) (c:tail) = if c == l
                                       then (l, f line) : scanlines f lines tail
                                       else (l, line) : scanlines f lines (c:tail)
scanlines f [] _ = []                                            
scanlines f t [] = t

-- Construct the search tree
maketree :: Int -> Int -> Board -> Tree
maketree r c puzzle = let updatedpuzzle = makenewboard puzzle r c
                          npuzzles = makec updatedpuzzle []
                          isfullyassigned = (length npuzzles) == 0
                          isvalid = issolved updatedpuzzle r c
                      in if isvalid then
                           if isfullyassigned then
                             (Node updatedpuzzle True [])
                           else
                             (Node [] False (map (maketree r c) npuzzles))
                         else
                           (Node [] False [])

makec :: Board -> Board -> [Board]
makec [] rem = []
makec (h:puzzle) rem = case h of
                         O -> makec puzzle (O:rem)
                         X -> makec puzzle (X:rem)
                         E -> makech (O:puzzle) rem :
                              makech (X:puzzle) rem :
                              makec puzzle (E:rem)

makech :: Board -> Board -> Board
makech [] rem = reverse rem
makech (h:puzzle) rem = makech puzzle (h:rem)

-- This finds the fixpoint of the function f
fix f x = let x' = f x
          in if x == x'
             then x
             else fix f x'

----------------------------------------------------
-- FUNCTIONS FOR FINDING THE SOLUTION IN THE TREE --
----------------------------------------------------
             
tsolve :: Int -> Int -> Board
tsolve r c = solve (replicate (r*c) E) r c

-- This function finds the solution to a given puzzle of size s
solve :: Board -> Int -> Int -> Board
solve puzzle r c = head (traversetree (maketree r c puzzle))

--solve :: Board -> Int -> Int -> Board
--solve puzzle r c = let tree = maketree r c puzzle in
--                     let solutions = traversetree tree in
--                       findsolution solutions r c

-- Traverse the search tree
traversetree :: Tree -> [Board]
traversetree (Node x True []) = [x]
traversetree (Node x False []) = []
traversetree (Node x bol xs) = t xs
  where
    t ((Node x bol xs):tail) = traversetree (Node x bol xs) ++ t tail
    t [] = []

findsolution :: [Board] -> Int -> Int -> Board
findsolution [] r c = []
findsolution (f:tail) r c = case issolved f r c of
                              True -> f
                              False -> findsolution tail r c

dfs :: Tree -> Maybe Board
dfs (Node x True []) = Just x
dfs (Node x False []) = Nothing
dfs (Node x bol xs) = w xs
  where
    w (h:tail) = case dfs h of
                   Just y -> Just y
                   Nothing -> w tail

----------------------------------
-- COMPUTE VALIDITY OF SOLUTION --
----------------------------------

-- Returns true if a puzzle is VALID: It obeys the three rules of the game.
-- It ignores undecided values completely
issolved :: Board -> Int -> Int -> Bool
issolved puzzle r c = let hslices = splitEvery c puzzle
                          vslices = transpose (splitEvery c puzzle)
                      in not (hastriples hslices vslices) &&
                         not (hasimbalance hslices vslices) &&
                         not (hasduplicates hslices vslices)

-- Returns true if the slices have triples
hastriples :: [Line] -> [Line] -> Bool
hastriples hslices vslices = any (checkfortriples) hslices || any (checkfortriples) vslices

-- Returns true if the numbers of 0 and 1 is different in a row or column
hasimbalance :: [Line] -> [Line] -> Bool
hasimbalance hslices vslices = any (checkforimbalance) hslices || any (checkforimbalance) vslices

-- Returns true if a row is a copy of another, or a column is a copy of another
hasduplicates :: [Line] -> [Line] -> Bool
hasduplicates hslices vslices = let phslices = filter (not . any(==E)) hslices -- Ignore all rows with undecided values in them
                                    pvslices = filter (not . any(==E)) vslices
                                in length phslices /= length (nub phslices) ||
                                   length pvslices /= length (nub pvslices)

hasnotbeenassigned :: [Line] -> [Line] -> Bool
hasnotbeenassigned hslices vslices = (any (any(==E)) hslices)

checkfortriples :: Line -> Bool
checkfortriples (x:y:z:tail) = if x==y && y==z && x/=E
                               then True
                               else checkfortriples (y:z:tail)
checkfortriples l = False

checkforimbalance :: Line -> Bool
checkforimbalance l = if not (any(==E) l) then
                        length (filter (==O) l) /= length (filter (==X) l)
                      else
                        length (filter (==O) l) > div (length l) 2 ||
                        length (filter (==X) l) > div (length l) 2

----------------------------------------
-- IMPLEMENTATION OF BASIC TECHNIQUES --
----------------------------------------

basic12 :: Line -> Line
basic12 line = basic12h [] line

basic12h :: Line -> Line -> Line
basic12h acc [] = reverse acc
basic12h acc (x : y : z : xs) = let (a,b,c) = basic12app x y z
                                in basic12h (a : acc) ( b : c : xs)
basic12h acc (z : xs) = basic12h (z : acc) xs

basic12app :: Slot -> Slot -> Slot -> (Slot, Slot, Slot)
basic12app x y E = if (x==y) then (x, y, nextMove x) else (x,y,E)
basic12app E x y = if (x==y) then (nextMove x, x, y) else (E,x,y)
basic12app x E y = if (x==y) then (x, nextMove x, y) else (x,E,y)
basic12app x y z = (x,y,z)

basic3 :: Line -> Line
basic3 line = let xr = m - length (filter (==X) line)
                  or = m - length (filter (==O) line)
                  m = div (length line) 2
              in if xr==2 && or==1 then
                   basic3getline line [[O,X,X],[X,O,X],[X,X,O]]
                 else if xr==1 && or==2 then
                        basic3getline line [[X,O,O],[O,X,O],[O,O,X]]
                      else
                        line

basic3getline line slots = let good = basic3makepermsh [] line slots
                           in if length good == 1 then -- TODO check this
                                let d = basic3diff [] (head good) (good!!1) -- This is ugly
                                in basic3combine [] line d
                              else
                                line

basic3makepermsh :: [Line]  -> Line -> [[Slot]] -> [[Slot]]
basic3makepermsh good line [] = good
basic3makepermsh good line (s:slots) = let new = basic3combine [] line s
                                           in if checkfortriples new then
                                                basic3makepermsh good line slots
                                              else
                                                basic3makepermsh (s:good) line slots

basic3combine :: Line -> Line -> [Slot] -> Line
basic3combine acc (E:line) (hs:slots) = basic3combine (hs:acc) line slots
basic3combine acc (hl:line) (hs:slots) = basic3combine (hl:acc) line (hs:slots)
basic3combine acc (hl:line) [] = basic3combine (hl:acc) line []
basic3combine acc [] [] = reverse acc

-- (HUnit) Insert the list of slots into the first available slots in the line
mergeinto :: [Slot] -> Line -> Line 
mergeinto (s:slots) (E:line) = s : mergeinto slots line
mergeinto (s:slots) (O:line) = O : mergeinto (s:slots) line
mergeinto (s:slots) (X:line) = X : mergeinto (s:slots) line
mergeinto [] line = line
mergeinto _ [] = [] -- This should never happen

basic3diff :: [Slot] -> [Slot] -> [Slot] -> [Slot]
basic3diff acc (x:t1) (y:t2)
  | x == y    = basic3diff (x:acc) t1 t2
  | otherwise = basic3diff (E:acc) t1 t2
basic3diff acc [] [] = reverse acc

-- Complete a line
basic4 :: Line -> Line
basic4 line = let xc = length (filter (==X) line)
                  oc = length (filter (==O) line)
                  m = div (length line) 2
              in if oc == m then
                   map (swap X) line
                 else
                   if xc == m then
                     map (swap O) line
                   else
                     line

-- Note that it takes tuples of Int and Line where Int is row/col #
-- Avoiding row or column duplication
basic5 :: [(Int, Line)] -> [(Int, Line)]
basic5 tups = let twoleft = [pos | (pos, line) <- tups, (countSymbol E line) == 2]
                  complete = [line | (pos, line) <- tups, (countSymbol E line) == 0]
              in let modified = [(p, if elem p twoleft then compareandset ln complete else ln) | (p, ln) <- tups]
                 in
                   modified --sortBy (compare `on` fst) (complete ++ twoleft)

compareandset :: Line -> [Line] -> Line
compareandset ln compl = last (scanl (compareandfix) ln compl)

compareandfix :: Line -> Line -> Line
compareandfix list1 list2 = compareandfixh list1 list1 list2 []
  
compareandfixh :: Line -> Line -> Line -> Line -> Line
compareandfixh original [] [] new = reverse new
compareandfixh original (sym1:tail1) (sym2:tail2) new = if sym1 == sym2 then
                                                         compareandfixh original tail1 tail2 (sym1:new)
                                                       else
                                                         if sym1 == E then
                                                           compareandfixh original tail1 tail2 ((nextMove sym2):new)
                                                         else
                                                           original

linediff :: Line -> Line -> [Int]
linediff ln1 ln2 = linediffh ln1 ln2 1 []

linediffh :: Line -> Line -> Int -> [Int] -> [Int]
linediffh [] [] pos acc = acc
linediffh (sym1:t1) (sym2:t2) pos acc = if sym1==sym2 then
                                          linediffh t1 t2 (pos+1) acc
                                        else
                                          linediffh t1 t2 (pos+1) (pos:acc)

-- This compares two lines and returns a list of the indices of all vals that are different
findchanges :: Line -> Line -> [Int]
findchanges l1 l2 = w l1 l2 [] 1
                    where
                      w (a:t1) (b:t2) acc ind = if a==b
                                                then w t1 t2 acc (ind+1)
                                                else w t1 t2 (ind:acc) (ind+1)
                      w [] [] rem ind = rem

findunassigned :: Line -> [Int]
findunassigned line = w line [] 1
                      where
                        w (E:t1) acc ind = w t1 (ind:acc) (ind+1)
                        w (h:t1) acc ind = w t1 acc (ind+1)
                        w [] acc ind = acc

scoop l1 l2 = let p = zip l1 l2
                  filt = (\(x,y) -> case (x,y) of
                             (E,n) -> False
                             (n,E) -> False
                             (n1,n2) -> n1/=n2)
              in filter (filt) p

--------------------------
-- ADVANCED TECHNIQUE 1 --
--------------------------

advanced1 :: [Line] -> [Line]
advanced1 lines = let len = length (head lines)
                  in map (advh len) lines

-- Apply the advanced 1 technique to a single line
advh :: Int -> Line -> Line
advh len line = let mx = len `div` 2 - countSymbol X line
                    mo = len `div` 2 - countSymbol O line
                in if mx==2 && mo>=2 then
                     f line X
                   else if mo==2 && mx>2
                        then f line O
                        else line

f :: Line -> Slot -> Line
f line sym = let l = tpFindEmpty line --List of empty indices
                 zl = zip [1..] line
             in [tryInsertAdv1 s p l line | (p,s) <- zl]

tryInsertAdv1 :: Slot -> Int -> [Int] -> Line -> Slot
tryInsertAdv1 sym pos empty line = let nline = replace line (pos, sym)
                                       l = tpFindEmpty nline
                                       testassignments = genAssign sym
                                   in sym
     
-- Generate all possible single assignments
genAssign :: Slot -> Int -> [Line]
genAssign sym len = map (h sym len) [1..len]

h :: Slot -> Int -> Int -> Line
h sym len i = let os = nextMove sym
                  l1 = i - 1
                  l2 = len - i
              in (replicate l1 os)++ [sym] ++ (replicate l2 os)            

replace :: Line -> (Int, Slot) -> Line
replace [] _ = []
replace (_:xs) (1,a) = a:xs
replace (x:xs) (n,a) = if n <= 0
                       then (x:xs)
                       else x: replace xs (n-1,a)


--------------------------
-- ADVANCED TECHNIQUE 2 --
--------------------------

advanced2 :: [Line] -> [Line]
advanced2 lines = let n = (length (head lines)) `div` 2
                      plines = numberLines lines
                      full = filter ((0==) . (countSymbol E)) lines
                      possible = remIrr n (remCom plines)
                  in lines --advanced2h full full

--advanced2h :: [NLine] -> [Line]-> [NLine]
advanced2h ((p, line):lines) complete = let sim = findAllSim line complete
                                        in (p, advanced2h' line sim) : advanced2h lines complete
advanced2h [] complete = []

advanced2h' :: Line -> [Line] -> Line
advanced2h' line complete = let mx = ((length line) `div` 2) - countSymbol X line
                                mo = ((length line) `div` 2) - countSymbol O line
                            in line

tp :: [Line] -> Line -> Line -> Slot -> Line
tp comp line acc sym = let eind = tpFindEmpty line -- List of empty position indices
                           indsym = tpTry eind line sym comp -- List of positions where to insert nextMove sym
                       in [tpSwap1 s sym p indsym | (p, s) <- zip [1..] line]

tpTry :: [Int] -> Line -> Slot -> [Line] -> [Int]
tpTry (p:positions) line sym comp = let nline = tpSwapInsert sym p line
                                    in if hasDup nline comp
                                       then p : tpTry positions line sym comp
                                       else tpTry positions line sym comp
tpTry [] line sym comp = []

tpSwap1 :: Slot -> Slot -> Int -> [Int] -> Slot
tpSwap1 O sym pos indices = O
tpSwap1 X sym pos indices = X
tpSwap1 E sym pos indices = if pos `elem` indices
                            then nextMove sym
                            else E

tpFindEmpty :: Line -> [Int]
tpFindEmpty line = h line 1
  where
    h (E:line) ind = ind : h line (ind+1)
    h (X:line) ind = h line (ind+1)
    h (O:line) ind = h line (ind+1)
    h [] ind = []

-- Takes a line and inserts given symbol at given position and
-- inserts the opposite symbol at every other free location
tpSwapInsert :: Slot -> Int -> Line -> Line
tpSwapInsert sym pos line = snd $ unzip $ map (h sym pos) (zip [1..] line)
  where
    h sym pos (p,O) = (p,O)
    h sym pos (p,X) = (p,X)
    h sym pos (p,E) = if pos == p
                      then (p,sym)
                      else (p,nextMove sym)

-- Returns true if the symbol can be inserted without duplicates
tryAndInsert :: Line -> Slot -> Line -> [Line] -> Bool
tryAndInsert front sym back comp = let l = map (swap (nextMove sym)) (back ++ (sym:front))
                                   in not $ hasDup l comp

-- (HUnit) Find all complete lines that the input line can be made into
findAllSim :: Line -> [Line] -> [Line]
findAllSim line complete = filter (canbemadeinto line) complete

numberLines :: [Line] -> [NLine]
numberLines lines = zip [1..] lines

deNumberLines :: [NLine] -> [Line]
deNumberLines lines = snd $ unzip lines

remInc :: [NLine] -> [NLine]
remInc lines = filter (\(p, line) -> (countSymbol E line) == 0) lines

remCom :: [NLine] -> [NLine]
remCom lines = filter (\(p, line) -> (countSymbol E line) > 0) lines

hasDup :: Line -> [Line] -> Bool
hasDup line complete = let m = line : complete
                       in length m /= length (nub m)

-- (HUnit)
remIrr :: Int -> [NLine] -> [NLine]
remIrr n ((p,line):lines) = let mx = n - (countSymbol X line)
                                mo = n - (countSymbol O line)
                            in if mx == 1 && mo >=2 then
                                 (p, line) : remIrr n lines
                               else
                                 if mx >= 2 && mo == 1 then
                                   (p, line) : remIrr n lines
                                 else
                                   remIrr n lines
remIrr _ [] = []


--------------------
-- TEST FUNCTIONS --
--------------------

--advanced2h' :: Line -> [Line] -> Line
--advanced2h' line complete = line

--test_advanced2h'1 = TestCase (assertEqual "for" []  (advanced2h' [X,O,E,E,E,O] [[X,O,O,X,O,X]]  ))

--tests_findAllSim = TestList [TestLabel "test1" test_findAllSim1,
--                             TestLabel "test2" test_findAllSim2]

test_tp1 = TestCase (assertEqual "for1" [O,X,X,O,O,X,E,O,X,E] (tp [[O,X,X,O,O,X,X,O,O,X]] [O,X,X,O,O,X,E,O,E,E] [] O))
test_tp2 = TestCase (assertEqual "for2" [O,O,X,O,E,E,X,X] (tp [[O,O,X,X,O,O,X,X]] [O,O,X,E,E,E,X,X] [] X))
test_tp3 = TestCase (assertEqual "for3" [O,X,E,O,E,X] (tp [[O,O,X,O,X,X]] [O,E,E,O,E,X] [] O))
test_tp4 = TestCase (assertEqual "for3" [X,O,E,E] (tp [[X,X,O,O]] [X,E,E,E] [] X))

tests_tp = TestList [TestLabel "test1" test_tp1,
                     TestLabel "test2" test_tp2,
                     TestLabel "test3" test_tp3,
                     TestLabel "test4" test_tp4]

test_findAllSim1 = TestCase (assertEqual "for" []                                   (findAllSim [X,X,E,E] [[X,O,X,O],[O,X,X,O],[O,O,X,O]]  ))
test_findAllSim2 = TestCase (assertEqual "for" [[X,X,X,O],[X,X,O,X],[X,X,O,O]]      (findAllSim [X,X,E,E] [[X,X,X,O],[X,X,O,X],[X,X,O,O]]  ))

tests_findAllSim = TestList [TestLabel "test1" test_findAllSim1,
                             TestLabel "test2" test_findAllSim2]

test_issolved1 = TestCase (assertEqual "for issolved [X,X,O,O] 2 2" False (issolved [X,X,O,O] 2 2  )) -- duplicates
test_issolved2 = TestCase (assertEqual "for issolved [X,O,O,O] 2 2" False (issolved [X,O,O,O] 2 2  )) -- imbalance
test_issolved3 = TestCase (assertEqual "for issolved [X,O,O,X] 2 2" True  (issolved [X,O,O,X] 2 2  ))
test_issolved4 = TestCase (assertEqual "for issolved [X] 1 1"       False (issolved [X] 1 1        ))
test_issolved5 = TestCase (assertEqual "for issolved [E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E] 4 4" True (issolved [E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E] 4 4))

tests_issolved = TestList [TestLabel "test1" test_issolved1, TestLabel "test2" test_issolved2,
                           TestLabel "test3" test_issolved3, TestLabel "test4" test_issolved4,
                           TestLabel "test5" test_issolved5]


test_basic12_1 = TestCase (assertEqual "for basic12 [O,O,E,E]" [O,O,X,E] (basic12 [O,O,E,E] ))
test_basic12_2 = TestCase (assertEqual "for basic12 [E,X,O,O]" [E,X,O,O] (basic12 [E,X,O,O] ))
test_basic12_3 = TestCase (assertEqual "for basic12 [O,X,O,E]" [O,X,O,E] (basic12 [O,E,O,E] ))
test_basic12_4 = TestCase (assertEqual "for basic12 [E,O,X,O]" [E,O,X,O] (basic12 [E,O,E,O] ))
test_basic12_5 = TestCase (assertEqual "for basic12 [X,O,O,X,O,X,X,O,X,O]" [X,O,O,X,O,X,X,O,X,O] (basic12 [E,O,O,E,O,X,X,E,E,O]))
test_basic12_6 = TestCase (assertEqual "for basic12 [E,E,E,E]" [E,E,E,E] (basic12 [E,E,E,E] ))

tests_basic12 = TestList [TestLabel "test1" test_basic12_1, TestLabel "test2" test_basic12_2,
                          TestLabel "test3" test_basic12_3, TestLabel "test4" test_basic12_4,
                          TestLabel "test5" test_basic12_5, TestLabel "test6" test_basic12_6]


test_hasduplicates1 = TestCase (assertEqual "for hasduplicates [[X,X],[O,O]] [[X,O],[X,O]]" True  (hasduplicates [[X,X],[O,O]] [[X,O],[X,O]] ))
test_hasduplicates2 = TestCase (assertEqual "for hasduplicates [[O,X],[X,O]] [[O,O],[X,X]]" False (hasduplicates [[O,X],[X,O]] [[O,O],[X,X]] ))
test_hasduplicates3 = TestCase (assertEqual "for hasduplicates [[E,E],[E,E]] [[E,E],[E,E]]" False (hasduplicates [[E,E],[E,E]] [[E,E],[E,E]] ))
test_hasduplicates4 = TestCase (assertEqual "for hasduplicates [[O,E],[O,E]] [[O,O],[E,E]]" False (hasduplicates [[O,E],[O,E]] [[O,O],[E,E]] ))

tests_hasduplicates = TestList [TestLabel "test1" test_hasduplicates1,
                                TestLabel "test2" test_hasduplicates2,
                                TestLabel "test3" test_hasduplicates3,
                                TestLabel "test4" test_hasduplicates4]


test_makec1 = TestCase (assertEqual "for makec [E,X,O,O] []" [[O,X,O,O],[X,X,O,O]] (makec [E,X,O,O] []  ))
test_makec2 = TestCase (assertEqual "for makec [X,X,O,E] []" [[X,X,O,O],[X,X,O,X]] (makec [X,X,O,E] []  ))
test_makec3 = TestCase (assertEqual "for makec [X,E,X,X] []" [[X,O,X,X],[X,X,X,X]] (makec [X,E,X,X] []  ))

tests_makec = TestList [TestLabel "test1" test_makec1,
                        TestLabel "test2" test_makec2,
                        TestLabel "test3" test_makec3]


test_checkforimbalance1 = TestCase (assertEqual "for checkforimbalance [E,X,O,O]" False (checkforimbalance [E,X,O,O]))
test_checkforimbalance2 = TestCase (assertEqual "for checkforimbalance [E,O,X,X]" False (checkforimbalance [E,O,X,X]))
test_checkforimbalance3 = TestCase (assertEqual "for checkforimbalance [E,O,O,O]" True (checkforimbalance [E,O,O,O]))
test_checkforimbalance4 = TestCase (assertEqual "for checkforimbalance [O,O,O,X]" True (checkforimbalance [O,O,O,X]))

tests_checkforimbalance = TestList [TestLabel "test1" test_checkforimbalance1,
                                    TestLabel "test2" test_checkforimbalance2,
                                    TestLabel "test3" test_checkforimbalance3,
                                    TestLabel "test4" test_checkforimbalance4]


test_mergeinto1 = TestCase (assertEqual "for mergeinto [O,O] [E,E,E,E]  " [O,O,E,E] (mergeinto [O,O] [E,E,E,E] ))
test_mergeinto2 = TestCase (assertEqual "for mergeinto [O,O] [X,X,E,E]  " [X,X,O,O] (mergeinto [O,O] [X,X,E,E] ))
test_mergeinto3 = TestCase (assertEqual "for mergeinto [O,O] [E,X,E,X]  " [O,X,O,X] (mergeinto [O,O] [E,X,E,X] ))
test_mergeinto4 = TestCase (assertEqual "for mergeinto [] [E,X,X,X]     " [E,X,X,X] (mergeinto []    [E,X,X,X] ))

tests_mergeinto = TestList [TestLabel "test1" test_mergeinto1, TestLabel "test2" test_mergeinto2,
                            TestLabel "test3" test_mergeinto3, TestLabel "test4" test_mergeinto4]

test_canbemadeinto1 = TestCase (assertEqual "for canbemadeinto [E,E] [O,O]" True  (canbemadeinto [E,E] [O,O]))
test_canbemadeinto2 = TestCase (assertEqual "for canbemadeinto [X,X] [X,X]" True  (canbemadeinto [X,X] [X,X]))
test_canbemadeinto3 = TestCase (assertEqual "for canbemadeinto [O,E] [X,E]" False (canbemadeinto [O,E] [X,E]))
test_canbemadeinto4 = TestCase (assertEqual "for canbemadeinto []    [X,E]" False (canbemadeinto []    [X,E]))
test_canbemadeinto5 = TestCase (assertEqual "for canbemadeinto [E,E] []"    False (canbemadeinto [E,E] []))

tests_canbemadeinto = TestList [TestLabel "test1" test_canbemadeinto1,
                                TestLabel "test2" test_canbemadeinto2,
                                TestLabel "test3" test_canbemadeinto3,
                                TestLabel "test4" test_canbemadeinto4,
                                TestLabel "test5" test_canbemadeinto5]

test_remIrr1 = TestCase (assertEqual "for remIrr" [(3,[X,E,E,E])]      (remIrr 2 [(1,[E,E,E,E]), (2,[X,O,O,E]), (3,[X,E,E,E])]))
test_remIrr2 = TestCase (assertEqual "for remIrr" []                   (remIrr 2 [(1,[E,E,E,E]), (2,[X,O,O,E]), (3,[X,X,E,E])]))
test_remIrr3 = TestCase (assertEqual "for remIrr" []                   (remIrr 2 [(1,[E,E,E,E]), (2,[X,O,O,E]), (3,[O,O,E,E])]))
test_remIrr4 = TestCase (assertEqual "for remIrr" [(1,[X,E,E,E,O,O])]  (remIrr 3 [(1,[X,E,E,E,O,O])]))
test_remIrr5 = TestCase (assertEqual "for remIrr" [(1,[O,E,E,E,X,X])]  (remIrr 3 [(1,[O,E,E,E,X,X])]))

tests_remIrr = TestList [TestLabel "test1" test_remIrr1,
                         TestLabel "test2" test_remIrr2,
                         TestLabel "test3" test_remIrr3,
                         TestLabel "test4" test_remIrr4,
                         TestLabel "test5" test_remIrr5]

--------------------
-- PROPERTY TESTS --
--------------------




