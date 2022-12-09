module Automaton (Automaton, randomAutomaton, exampleAutomaton, automatonToString) where
import qualified Data.Map as Map
import qualified Data.Set as Set

-- A node's "id"
type State = Int
-- A labeled edge pointing to a node
type Edge = (Int, State)
-- Successors map, initial states and final states
data Automaton = Automaton (Map.Map State [Edge]) (Set.Set State) (Set.Set State)

-- Example taken from https://en.wikipedia.org/wiki/Powerset_construction (5 states NFA generating a 16 states DFA through the algorithm)
initStates :: Set.Set State
initStates = Set.fromList [0]
finalStates :: Set.Set State
finalStates = Set.fromList [4]
successors :: Map.Map State [Edge]
successors = Map.fromList [(0, [(0, 0), (1, 0), (1, 1)]), (1, [(0, 2), (1, 2)]), (2, [(0, 3), (1, 3)]), (3, [(0, 4), (1, 4)]), (4, [])]
exampleAutomaton::Automaton
exampleAutomaton = Automaton successors initStates finalStates

randomAutomaton :: Int -> Int -> Int -> Int -> Float -> Automaton
randomAutomaton alphabetSize nbState nbInit nbFinal probability = Automaton Map.empty Set.empty Set.empty

automatonToString :: Automaton -> String
automatonToString = recToString ""
    where recToString str autom =  ""
