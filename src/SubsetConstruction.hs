module SubsetConstruction (nfaToDfa) where
import qualified Automaton as A
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

exploreLabelFromNFAState :: A.AdjacencyList -> A.Label -> A.State -> [A.State]
exploreLabelFromNFAState nfaAdjacency label state = [s | (l, s) <- edges, l == label]
    where edges = nfaAdjacency Map.! state

exploreLabelFromDFAState :: A.AdjacencyList -> A.Label -> Set.Set A.State -> Set.Set A.State
exploreLabelFromDFAState nfaAdjacency label =
    Set.fromList
    . List.concatMap (exploreLabelFromNFAState nfaAdjacency label)
    . Set.toList

epsilonClosure :: A.AdjacencyList -> Set.Set A.State -> Set.Set A.State
epsilonClosure nfaAdjacency nfaStates    | Set.size nfaStates ==  Set.size explored = nfaStates
                                        | otherwise = epsilonClosure nfaAdjacency explored
                    where explored = Set.union nfaStates (exploreLabelFromDFAState nfaAdjacency A.Epsilon nfaStates)

nextStates :: A.AdjacencyList -> [A.Label] -> [Set.Set A.State] -> [(A.Label, Set.Set A.State, Set.Set A.State)]
nextStates nfaAdjacency alphabet dfaStates = [
        (l, s, (epsilonClosure nfaAdjacency .  exploreLabelFromDFAState nfaAdjacency l) s) |
        s <- dfaStates,
        l <- alphabet
    ]

addDfaEdge :: (A.DfaStatesMap, A.AdjacencyList, [Set.Set A.State])
                -> (A.Label, Set.Set A.State, Set.Set A.State)
                -> (A.DfaStatesMap, A.AdjacencyList, [Set.Set A.State])
addDfaEdge ((dfaSM, maxIdx), dfaA, tV) (l, originS, destS) = case Map.lookup destS dfaSM of
                Nothing -> ((Map.insert destS newIdx dfaSM, newIdx), Map.insertWith (++) originIdx [(l, newIdx)] dfaA, destS : tV)
                    where newIdx = maxIdx + 1
                Just s -> ((dfaSM, maxIdx), Map.insertWith (++) originIdx [(l,s)] dfaA, tV)
            where originIdx = dfaSM Map.! originS

explore :: A.AdjacencyList -> Set.Set A.State -> [A.Label] -> A.DfaStatesMap -> A.AdjacencyList -> Set.Set A.State -> [Set.Set A.State] -> (A.AdjacencyList, Set.Set A.State)
explore _ _ _ _ dfaAdjacency dfaFinals []                                   = (dfaAdjacency, dfaFinals)
explore nfaAdjacency nfaFinals alphabet dfaStatesMap dfaAdjacency dfaFinals toVisit =
    explore nfaAdjacency nfaFinals alphabet newDfaSM newDfaA (((dfaFinals))) newToVisit
        where   (newDfaSM, newDfaA, newToVisit) =
                    List.foldl'
                        addDfaEdge
                        (dfaStatesMap, dfaAdjacency, [])
                        nStates
                nStates = nextStates nfaAdjacency alphabet toVisit

nfaToDfa :: A.Automaton -> A.Automaton
nfaToDfa (A.Automaton nfaAdjacency alphabet inits nfaFinals) = A.Automaton newAdjacency alphabet dfaInits dfaFinals
        where (newAdjacency, dfaFinals) = explore nfaAdjacency nfaFinals alphabet (initDfaStatesMap, 0) initDfaAdjacency Set.empty [inits]
              initDfaStatesMap =  Map.fromList [(inits, 0)]
              initDfaAdjacency =  Map.fromList [(0, [])]
              dfaInits = Set.fromList [0]
