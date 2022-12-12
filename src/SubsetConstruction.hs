module SubsetConstruction (nfaToDfa) where
import qualified Automaton  as Auto
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

exploreLabelFromState :: Map.Map Auto.State [Auto.Edge] -> Auto.Label -> Set.Set Auto.State -> Auto.State -> Set.Set Auto.State
exploreLabelFromState transitions label states state = List.foldl' addState states edges
    where edges = Maybe.fromMaybe [] (Map.lookup state transitions)
          addState currStates (l, s) | l == label = Set.insert s currStates
                                     | otherwise = currStates

exploreLabelFromStates :: Map.Map Auto.State [Auto.Edge] -> Auto.Label -> Set.Set Auto.State -> Set.Set Auto.State
exploreLabelFromStates transitions label states = List.foldl' (exploreLabelFromState transitions label) states (Set.toList states)

epsilonClosure :: Map.Map Auto.State [Auto.Edge] -> Set.Set Auto.State -> Set.Set Auto.State
epsilonClosure transitions states | Set.size states ==  Set.size explored = states
                                  | otherwise = epsilonClosure transitions explored
                    where explored = exploreLabelFromStates transitions Auto.Epsilon states

nfaToDfa :: Auto.Automaton -> Auto.Automaton
nfaToDfa automaton =  automaton
