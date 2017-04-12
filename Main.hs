import Language.HaLex.Dfa
import Language.HaLex.FaAsDiGraph
--import Data.GraphViz
--import Data.GraphViz
{-
data Dfsa st sy  = Dfsa  [ sy ]  -- Vocabulary
            [ st ]  -- Finite set of states
              st  -- The start state
            [ st ]  -- The set of final states
          (st -> sy -> st)  -- The transition function
-}
ex1  :: Dfa Int Char
ex1  = Dfa sym sta stt stf delta
      where  
        delta 1 'a' = 1
        delta 1 'b' = 2
        delta 2 'a' = 3
        delta _ _ = 4
	sym = ['a','b']
	sta = [1,2,3,4]
	stt = 1
	stf = [3]

strig::String
strig = dfa2DiGraphWithNoSyncSt ex1 "dot"

main = do
  print (strig)
  dfa2graphviz2file ex1 "out"
  --runGraphvizCommand dot Char dfa2graphviz ex1 
  --(Eq sy, Ord a, Show sy, Show a)
  -- => Dfa a sy -> [Char] -> IO (
