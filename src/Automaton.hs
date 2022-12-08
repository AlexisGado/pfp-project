module Automaton (Automaton, randomAutomaton) where
import qualified Data.Map as Map
import qualified Data.Set as Set

-- A node's "id"
type State = Int
-- A labeled edge pointing to a node
type Edge = (Int, State)
-- Successors map, initial states and final states
data Automaton = Automaton (Map.Map State [Edge]) (Set.Set State) (Set.Set State)

randomAutomaton :: Int -> Int -> Int -> Int -> Float -> Automaton
randomAutomaton alphabetSize nbState nbInit nbFinal density = Automaton Map.empty Set.empty Set.empty
