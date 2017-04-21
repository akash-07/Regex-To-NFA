import Language.HaLex.Ndfa
import Language.HaLex.Dfa
import Language.HaLex.FaAsDiGraph
import Data.List
import Data.Char
import RegexToDfa
{-
data Dfsa st sy  = Dfsa  [ sy ]  -- Vocabulary
            [ st ]  -- Finite set of states
              st  -- The start state
            [ st ]  -- The set of final states
          (st -> sy -> st)  -- The transition function
-}

sym :: [Char]
sym=['a','b']

sta :: [Int]
sta = [1,2,3,4]

stt :: Int
stt=1

stf::[Int]
stf=[3]

ex1  :: Dfa Int Char
ex1  = Dfa sym sta stt stf delta1
      where  
        delta1 1 'a' = 1
        delta1 1 'b' = 2
        delta1 2 'a' = 3
        delta1 _ _   = 4

clex :: String -> [Token]
clex (c:cs)	| isDigit c = num_token : clex rest_num_token
		| isAlpha c = var_token : clex rest_var_token
		| otherwise = [c]:clex cs where
			var_token = c:takeWhile isIdChar cs
			num_token = c:takeWhile isDigit cs
			rest_num_token = dropWhile isDigit cs
			rest_var_token = dropWhile isIdChar cs
clex [] = []

isIdChar :: Char -> Bool
isIdChar c= isAlphaNum c || c=='_'

type Token = String

convert :: [Token] -> [Token]
convert [] =[]
convert [x] = [x]
convert (x:y:xs) = if(x=="\"" && y=="\"") then x:(convert xs) 
			else x:(convert (y:xs))

final :: [Token]->String
final [] = []
final (x:xs) = x++(final xs)

strig::String
strig = ndfa2graphviz myNFA "dot"

myNFA :: Ndfa Int Char
myNFA = Ndfa ['a','b'] (flatten states) [start] (flatten fin) delta  

list1 :: [Move Int] -> [(Int,Char,Int)]
list1 [] = []
list1 (Move x c y:xs) = (x,c,y):(list1 xs)
list1 (EMove x y:xs) = (x,'z',y):(list1 xs)
{-
list2 :: [Move Int] -> [Char]
list2 [] = []
list2 (Move x c y:xs) = c:(list2 xs)
list2 (EMove x y:xs) = 'z':(list2 xs)

list3 :: [Move Int] -> [Int]
list3 [] = []
list3 (Move x c y:xs) = y:(list3 xs)
list3 (EMove x y:xs) = y:(list3 xs)

// 2,z --> list1 --> () 
-}
list2 :: [(Int,Char,Int)]
list2 = list1 (flatten moves)

delta :: Int -> Maybe Char -> [Int]
delta x (Just c) =  [n | (m,ch,n) <- list2, m == x , ch == c , ch /= 'z']
delta x Nothing =   [n | (m,ch,n) <- list2, m == x , ch == 'z']
		
		

main = do
  --print (strig)
  {- 
	a+b
  ndfa2graphviz2file myNFA "aOrbq"
  x<- readFile "aOrbq.dot"
  writeFile "aOrb.dot" (final (convert (clex x)))
-}

{-
	Only a
  ndfa2graphviz2file myNFA "aq"
  x<- readFile "aq.dot"
  writeFile "a.dot" (final (convert (clex x)))
-}

{-
	a.b
  ndfa2graphviz2file myNFA "abq"
  x<- readFile "abq.dot"
  writeFile "ab.dot" (final (convert (clex x)))
-}

{-
	(a*)
  ndfa2graphviz2file myNFA "a_starq"
  x<- readFile "a_starq.dot"
  writeFile "a_star.dot" (final (convert (clex x)))
-}

  -- (a+ab)*
  ndfa2graphviz2file myNFA "comboq"
  x<- readFile "comboq.dot"
  writeFile "combo.dot" (final (convert (clex x)))
   --print (show (convert (clex x)))
  --runGraphvizCommand dot Char dfa2graphviz ex1 
  --(Eq sy, Ord a, Show sy, Show a)
  -- => Dfa a sy -> [Char] -> IO (


