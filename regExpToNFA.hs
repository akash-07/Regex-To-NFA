import Data.List hiding (union)

---------------------------------------------------------------
-- REGULAR EXPRESSIONS

-- A new data type for regular expressions
data Reg = 	Epsilon | 
			Literal Char | 
			Or Reg Reg | 
			Then Reg Reg |
			Star Reg
			deriving Eq

-- literals prints all the characters in an a regular expression  
literals :: Reg -> [Char]
literals Epsilon = []
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r) = literals r

-- printRE prints the regular expressions in a readable form
printRE :: Reg -> [Char]
printRE Epsilon = "@"
printRE (Literal ch) = [ch]
printRE (Or r1 r2) = "(" ++ printRE r1 ++ "|" ++ printRE r2 ++ ")"
printRE (Then r1 r2) = "(" ++ printRE r1 ++ printRE r2 ++ ")"
printRE (Star r) = "(" ++ printRE r ++ ")*"

instance Show Reg where
	show = printRE
-----------------------------------------------------------------
-- MATCHING STRINGS AND REGULAR EXPRESSIONS

-- splits function splits the list into in all possible ways
splits :: [a] -> [([a],[a])]
splits str = [splitAt n str | n <- [0 .. length str]]

-- frontSplits function splits the list in all possible ways except
-- for ([],str)
frontSplits :: [a] -> [([a],[a])]
frontSplits str = [splitAt n str | n <- [1 .. length str]]

-- matches takes in a regular and a string returns true if the string 
-- matches the regex
matches :: Reg -> String -> Bool
matches Epsilon str = (str == "")
matches (Literal ch) str = (str == [ch])
matches (Or r1 r2) str = matches r1 str || matches r2 str
matches (Then r1 r2) str = 
	or [matches r1 s1 && matches r2 s2 |(s1,s2) <- splits str]
matches (Star r) str = matches Epsilon str ||	
	or [matches r s1 && matches (Star r) s2 |(s1,s2) <- frontSplits str]

--------------------------------------------------------------------
-- SETS

-- A set of type a contains a list of values of type a named by
-- constructor Set1. Also we assume that there is an ordering in the set
-- as is given by the indices of elements in the list and repetitons are
-- not allowed
data Set a = Set1 [a]

-- empty set is just an empty list
empty :: Set a
empty = Set1 []

-- singleton set takes in an element and gives out a set with just
-- that element
sing :: a -> Set a
sing x = Set1 [x]

-- memsSet checks whether y is a member of given set
memSet :: Ord a => Set a -> a -> Bool
memSet (Set1 []) y = False
memSet (Set1 (x:xs)) y 
	| x<y  = memSet (Set1 xs) y
	| x == y = True
	| otherwise = False

union :: Ord a => Set a -> Set a -> Set a
union (Set1 xs) (Set1 ys) = Set1 (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys) 
	| x<y = x : uni xs (y:ys)
	| x == y = x : uni xs ys
	| otherwise = y : uni (x:xs) (ys)

inter :: Ord a => Set a -> Set a -> Set a
inter (Set1 xs) (Set1 ys) = Set1 (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys) 
	| x<y = int xs (y:ys)
	| x == y = x : int xs ys
	| otherwise = int (x:xs) (ys)

diff :: Ord a => Set a -> Set a -> Set a
diff (Set1 xs) (Set1 ys) = Set1 (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a]
dif [] ys = []
dif xs [] = xs
dif (x:xs) (y:ys) 
	| x<y = x : dif xs (y:ys)
	| x == y = dif xs ys
	| otherwise = dif (x:xs) ys

-- ! ALERT ! -> Keep the elements ordered when making sets
{-
a :: Set Int
a = Set1 [2,4,5,6,7,8,9]
b :: Set Int
b = Set1 [5,6,7]
-}

showSet :: Show a => Set a -> String
showSet (Set1 xs) = show xs

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set1 xs) (Set1 ys) = subS xs ys 

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys  = True
subS xs [] = False
subS (x:xs) (y:ys)
	| x<y = False
	| x == y = subS xs ys
	| x>y = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set1 xs) (Set1 ys) = (xs ==  ys)

-- Do we need to use makeSet here or this is correct?
-- Yes, because the new list might contain repeated elements or it
-- might not be ordered which is required for our set
mapSet :: Ord b => (a -> b) -> Set a -> Set b
-- mapSet f (Set1 xs) = Set1 (map f xs) can be written as
mapSet f (Set1 xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet f (Set1 xs) = Set1 (filter f xs)

-- Check what foldr exactly does!
foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (Set1 xs) = (foldr f x xs)

makeSet :: Ord a => [a] -> Set a
makeSet = Set1. remDups. sort where
			remDups [] = []
			remDups [x] = [x]
			remDups (x:y:xs)
				| x<y = x : remDups (y:xs)
				| otherwise = remDups (y:xs)

card :: Set a -> Int
card (Set1 xs) = length xs

flatten :: Set a -> [a]
flatten (Set1 xs) = xs

setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s 
	| eqSet s next = s
	| otherwise = setlimit f next
	where 
		next = f s
----------------------------------------------------------------------
-- NFA 

-- Eq not defined on the data type Nfa a 
data Nfa a = NFA (Set a) 
				(Set (Move a)) 
				a 
				(Set a)

printNFA :: Show a => Nfa a -> IO()
printNFA (NFA states moves start final)	
	= putStr ("States:\n" ++ (showSet states) ++ "\n\n" ++ 
	"Moves:\n" ++ showMoves moves ++ "\n" ++
	"Start State:\n" ++ (show start) ++ "\n\n" ++
	"Final States:\n" ++ (showSet final) ++ "\n")

showMoves :: Show a => Set (Move a) -> String
showMoves (Set1 []) = ""
showMoves (Set1 (x:xs)) = (show x) ++ "\n" ++ showMoves (Set1 xs)	

-- Eq and ordring not defined on the data type Move a
data Move a = Move a Char a | 
			  EMove a a 
			  deriving (Eq,Ord,Show)

-- name changed from foldl to fold_l
fold_l :: (Set a -> Char-> Set a) -> Set a -> String -> Set a
fold_l f r [] = r
fold_l f r (c:cs) = fold_l f (f r c) cs

onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA states moves start final) c x
	= makeSet [s | t <- flatten x, (Move z d s) <- flatten moves, d == c, z == t]

closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA states moves start final)
 	= setlimit add
 		where
 		add stateset = union stateset (makeSet accessible) 
 						where
 						accessible
 							= [s | t <- flatten stateset, EMove y s <- flatten moves, y == t]

trans :: Ord a => Nfa a -> String -> Set a
trans mach str = fold_l step startset str
					where
						step set ch = onetrans mach ch set
						startset = closure mach (sing (startstate mach)) 


onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

startstate :: Nfa a -> a
startstate (NFA states moves start final) = start

machine :: Nfa Int
machine = NFA (makeSet [0 .. 3])
			(makeSet [	Move 0 'a' 0,
						Move 0 'a' 1,
						Move 0 'b' 0,
						Move 1 'b' 2,
						Move 2 'b' 3])
			0
			(sing 3)

-----------------------------------------------------------------

build :: Reg -> Nfa Int

build (Literal c) = NFA
					(makeSet [0 .. 1])
					(sing (Move 0 c 1))
					0
					(sing 1)

build Epsilon = NFA
				(makeSet [0 .. 1])
				(sing (EMove 0 1))
				0
				(sing 1)
				
build (Or r1 r2) = m_or (build r1) (build r2)
build (Then r1 r2) = m_then (build r1) (build r2)
build (Star r) = m_star (build r)

m_or :: Nfa Int -> Nfa Int -> Nfa Int
m_or 	(NFA states1 moves1 start1 finish1)
		(NFA states2 moves2 start2 finish2)
		= NFA 
			(states1' `union` states2' `union` newstates)
			(moves1' `union` moves2' `union` newmoves)
			0
			(sing (m1+m2+1))
			where
				m1 = card states1
				m2 = card states2
				states1' = mapSet (renumber 1) states1
				states2' = mapSet (renumber (m1+1)) states2
				newstates = makeSet [0,(m1+m2+1)]
				moves1' = mapSet (renumber_move 1) moves1
				moves2' = mapSet (renumber_move (m1+1)) moves2
				newmoves = makeSet [EMove 0 1, EMove 0 (m1+1),
									EMove m1 (m1+m2+1),
									EMove (m1+m2) (m1+m2+1)]

m_then :: Nfa Int -> Nfa Int -> Nfa Int
m_then 	(NFA states1 moves1 start1 finish1)
		(NFA states2 moves2 start2 finish2)
		= NFA 
			(states1' `union` states2' `union` newstates)
			(moves1' `union` moves2' `union` newmoves)
			0
			(sing (m1+m2+1))
			where
				m1 = card states1
				m2 = card states2
				states1' = mapSet (renumber 1) states1
				states2' = mapSet (renumber (m1+1)) states2
				newstates = makeSet [0,(m1+m2+1)]
				moves1' = mapSet (renumber_move 1) moves1
				moves2' = mapSet (renumber_move (m1+1)) moves2
				newmoves = makeSet [EMove 0 1,
									EMove m1 (m1+1),
									EMove (m1+m2) (m1+m2+1)]

m_star :: Nfa Int -> Nfa Int
m_star 	(NFA states1 moves1 start1 finish1)
		= NFA 
			(states1' `union` newstates)
			(moves1' `union` newmoves)
			0
			(sing (m1+1))
			where
				m1 = card states1
				states1' = mapSet (renumber 1) states1
				newstates = makeSet [0,(m1+1)]
				moves1' = mapSet (renumber_move 1) moves1
				newmoves = makeSet [EMove 0 1,
									EMove m1 (m1+1),
									EMove 0 (m1+1),
									EMove m1 1]

renumber ::Int -> Int -> Int
renumber num = (+num)

renumber_move :: Int -> Move Int -> Move Int
renumber_move offset (Move x c y) = Move (x+offset) c (y+offset)
renumber_move offset (EMove x y) = EMove (x+offset) (y+offset)

----------------------------------------------------------------
a = Literal 'a'
b = Literal 'b'
reg :: Reg
reg = (Then (Then (Star a) (Star b)) (Star a))

------------------------------------------------------------------