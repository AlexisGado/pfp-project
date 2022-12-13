module SubsetConstruction (nfaToDfa) where
import qualified Automaton  as A
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

exploreLabelFromNFAState :: A.AdjacencyList -> A.Label -> A.State -> [A.State]
exploreLabelFromNFAState nfaAdjacency label state = [s | (l, s) <- edges, l == label]
    where edges = Maybe.fromMaybe [] (Map.lookup state nfaAdjacency)

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

tempRec :: A.AdjacencyList -> [A.Label] -> Map.Map (Set.Set A.State) A.State -> A.AdjacencyList -> [Set.Set A.State] -> A.AdjacencyList
tempRec _ _ _ dfaAdjacency []                                   = dfaAdjacency
tempRec nfaAdjacency alphabet dfaStatesMap dfaAdjacency toVisit =
    tempRec nfaAdjacency alphabet newDfaSM newDfaA newToVisit
        where   (newDfaSM, newDfaA, newToVisit) =
                    List.foldl'
                        addDfaEdge
                        (dfaStatesMap, dfaAdjacency, [])
                        nStates
                nStates = nextStates nfaAdjacency alphabet toVisit
                addDfaEdge (dfaSM, dfaA, tV) (l, originS, destS) = case Map.lookup destS dfaSM of
                        Nothing -> (Map.insert destS 42 dfaSM, Map.insertWith (++) originIdx [(l, 42)] dfaA, destS : tV)
                        Just s -> (dfaSM, Map.insertWith (++) originIdx [(l,s)] dfaA, tV)
                    where originIdx = dfaSM Map.! originS



nfaToDfa :: A.Automaton -> A.Automaton
nfaToDfa (A.Automaton nfaAdjacency alphabet inits finals) = A.Automaton nfaAdjacency alphabet inits finals
