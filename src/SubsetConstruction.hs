module SubsetConstruction (nfaToDfa) where
import qualified Automaton                   as A
import           Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set

exploreLabelFromNFAState :: A.AdjacencyList -> A.Label -> A.State -> [A.State]
exploreLabelFromNFAState nfaAdjacency label state = [s | (l, s) <- edges, l == label]
    where edges = fromMaybe [] (Map.lookup state nfaAdjacency)

exploreLabelFromDFAState :: A.AdjacencyList -> A.Label -> Set.Set A.State -> Set.Set A.State
exploreLabelFromDFAState nfaAdjacency label =
    Set.fromList
    . List.concatMap (exploreLabelFromNFAState nfaAdjacency label)
    . Set.toList

epsilonClosure :: A.AdjacencyList -> Set.Set A.State -> Set.Set A.State
epsilonClosure nfaAdjacency nfaStates    | Set.size nfaStates ==  Set.size explored = nfaStates
                                        | otherwise = epsilonClosure nfaAdjacency explored
                    where explored = Set.union nfaStates (exploreLabelFromDFAState nfaAdjacency A.Epsilon nfaStates)

-- nextStates :: A.AdjacencyList -> [A.Label] -> [Set.Set A.State] -> [(A.Label, Set.Set A.State, Set.Set A.State)]
-- nextStates nfaAdjacency alphabet dfaStates = [
--         (l, s, (epsilonClosure nfaAdjacency .  exploreLabelFromDFAState nfaAdjacency l) s) |
--         s <- dfaStates,
--         l <- alphabet
--     ] `using` parList r0

nextStates :: A.AdjacencyList -> [A.Label] -> [Set.Set A.State] -> [(A.Label, Set.Set A.State, Set.Set A.State)]
nextStates nfaAdjacency alphabet dfaStates =
        parMap rdeepseq
        (\(l, s) -> (l, s, (epsilonClosure nfaAdjacency .  exploreLabelFromDFAState nfaAdjacency l) s))
        [(l,s) |
            l <- alphabet,
            s <- dfaStates
        ]

addDfaEdge ::  Set.Set A.State -> (A.DfaStatesMap, A.AdjacencyList, Set.Set A.State, [Set.Set A.State])
                -> (A.Label, Set.Set A.State, Set.Set A.State)
                -> (A.DfaStatesMap, A.AdjacencyList, Set.Set A.State, [Set.Set A.State])
addDfaEdge nfaFinals ((dfaSM, maxIdx), dfaA, dfaF, tV) (l, originS, destS) = case Map.lookup destS dfaSM of
                Nothing | Set.null destS -> ((dfaSM, maxIdx), dfaA, dfaF, tV)
                Nothing -> ((Map.insert destS newState dfaSM, newState), Map.insertWith (++) originState [(l, newState)] dfaA, newDfaF, destS : tV)
                    where newState = maxIdx + 1
                          isFinal = List.any (`Set.member` nfaFinals) (Set.toList destS)
                          newDfaF = if isFinal then Set.insert newState dfaF else dfaF
                Just s -> ((dfaSM, maxIdx), Map.insertWith (++) originState [(l,s)] dfaA, dfaF, tV)
            where originState = case Map.lookup originS dfaSM of
                    Just st -> st
                    Nothing -> error "Couldn't find origin state"

explore :: A.AdjacencyList -> Set.Set A.State -> [A.Label] -> A.DfaStatesMap -> A.AdjacencyList -> Set.Set A.State -> [Set.Set A.State] -> (A.AdjacencyList, Set.Set A.State)
explore _ _ _ _ dfaAdjacency dfaFinals []                                   = (dfaAdjacency, dfaFinals)
explore nfaAdjacency nfaFinals alphabet dfaStatesMap dfaAdjacency dfaFinals toVisit =
    explore nfaAdjacency nfaFinals alphabet newDfaSM newDfaA newDfaFinals newToVisit
        where   (newDfaSM, newDfaA, newDfaFinals, newToVisit) =
                    List.foldl'
                        (addDfaEdge nfaFinals)
                        (dfaStatesMap, dfaAdjacency, dfaFinals, [])
                        nStates
                nStates = nextStates nfaAdjacency alphabet toVisit

nfaToDfa :: A.Automaton -> A.Automaton
nfaToDfa (A.Automaton nfaAdjacency alphabet inits nfaFinals) = A.Automaton newAdjacency alphabet dfaInits dfaFinals
        where (newAdjacency, dfaFinals) = explore nfaAdjacency nfaFinals alphabet (initDfaStatesMap, 0) initDfaAdjacency Set.empty [inits]
              initDfaStatesMap =  Map.fromList [(inits, 0)]
              initDfaAdjacency =  Map.fromList [(0, [])]
              dfaInits = Set.fromList [0]
