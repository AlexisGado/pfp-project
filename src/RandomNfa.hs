module RandomNfa (randomNFA) where
import           Automaton     (Label (..))
import qualified Automaton     as A
import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified System.Random as Random



-- Inspired from https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html
rolls :: Int -> Int -> Int -> [Int]
rolls n maxInt seed = take n . List.unfoldr (Just . Random.uniformR (0, maxInt)) $ Random.mkStdGen seed

-- Generate a random NFA with a given number of states, an alphabet size, a number of final states and a probability
-- (probability that there is an edge with a given label betwen 2 given states)
randomNFA :: Int -> Int -> Int -> Int -> A.Automaton
randomNFA numStates alphabetSize nbFinals proba =
    A.Automaton transitions alphabet initStates finalStates
    where

        intForGen = numStates + alphabetSize + nbFinals + proba

        -- list of states
        states = [0..(numStates-1)]
        -- final states
        finalStates = Set.fromList [(numStates - nbFinals)..(numStates-1)]
        -- alphabet
        alphabet = [Label i | i <- [0..(alphabetSize - 1)]]
        -- alphabetWithEps = Epsilon : alphabet

        allTransitions = [
            (state1, symbol, state2) |
            state1 <- states,
            state2 <- states,
            symbol <- alphabet ]
        -- allTransitionsEps = [
        --     (state1, A.Epsilon, state2) |
        --     state1 <- states,
        --     state2 <- states]

        keepTransitions = [ rdInt <= proba | rdInt <- rolls (length allTransitions) 100 intForGen]
        -- keepTransitionsEps = [ rdInt <= probaEps | rdInt <- rolls (length allTransitions) 1000 intForGen]

        -- transitionsListEps = [ v | (v,keep) <- List.zip allTransitionsEps keepTransitionsEps, keep]
        transitionsList = [ v | (v,keep) <- List.zip allTransitions keepTransitions, keep]

        -- turn the list into a map
        transitions :: A.AdjacencyList
        transitions =
            List.foldl'
                (\m (fromS, label, toS) -> Map.insertWith (++) fromS [(label, toS)] m )
                Map.empty
                transitionsList

        -- Generate a random start state
        initStates = Set.singleton 0
