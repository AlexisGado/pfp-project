module Automaton (Automaton(..), AdjacencyList, DfaStatesMap, State, Edge, Label(..), exampleAutomaton, exampleAutomaton2, ioDumbAutomaton) where
import           Control.DeepSeq (NFData (rnf))
import qualified Data.List       as List
import qualified Data.Map        as Map
import qualified Data.Set        as Set

-- A node's "id"
type State = Int
-- A labeled edge pointing to a node
data Label = Epsilon | Label !Int deriving (Eq, Show, Ord)

instance NFData Label where
  rnf (Label x) = rnf x
  rnf Epsilon   = ()

type Edge = (Label, State)
-- Successors map, initial states and final states
type AdjacencyList = Map.Map State [Edge]
data Automaton = Automaton !AdjacencyList ![Label] !(Set.Set State) !(Set.Set State) deriving (Show)

type DfaStatesMap = (Map.Map (Set.Set State) State, Int)

-- Example taken from https://en.wikipedia.org/wiki/Powerset_construction (5 states NFA generating a 16 states DFA through the algorithm)
alphabet :: [Label]
alphabet = [Label 0, Label 1]
initStates :: Set.Set State
initStates = Set.fromList [0]
finalStates :: Set.Set State
finalStates = Set.fromList [4]
successors :: AdjacencyList
successors = Map.fromList [(0, [(Label 0, 0), (Label 1, 0), (Label 1, 1)]), (1, [(Label 0, 2), (Label 1, 2)]), (2, [(Label 0, 3), (Label 1, 3)]), (3, [(Label 0, 4), (Label 1, 4)]), (4, [])]
exampleAutomaton :: Automaton
exampleAutomaton = Automaton successors alphabet initStates finalStates
initStates2 :: Set.Set State
initStates2 = Set.fromList [0]
finalStates2 :: Set.Set State
finalStates2 = Set.fromList [2]
successors2 :: AdjacencyList
successors2 = Map.fromList [(0, [(Label 0, 1)]), (1, [(Label 1, 2)]), (2, [(Epsilon, 0)])]
exampleAutomaton2 :: Automaton
exampleAutomaton2 = Automaton successors2 alphabet initStates2 finalStates2

intAlphabet :: [Label]
intAlphabet = List.map Label [0..50]

dumbAutomaton :: Int -> Automaton
dumbAutomaton nStates = Automaton adj intAlphabet (Set.singleton 0) (Set.singleton $ nStates - 1)
            where adj = Map.insert 0 [(Label 0, 0), (Label 1, 0), (Label 1, 1)] allButFirstSucc
                  allButFirstSucc = Map.insert (nStates-1) [] allButFirstAndLast
                  allButFirstAndLast = Map.fromList $ List.map (\n -> (n, [(l, r) | l <- intAlphabet, r <- [n+1,n,  n-1]])) [1..nStates-2]

ioDumbAutomaton :: Int -> IO Automaton
ioDumbAutomaton n = return $ dumbAutomaton n
