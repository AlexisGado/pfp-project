module Dictionary (dictNfa) where

import           Automaton (AdjacencyList, Automaton (..), Label (..), State)
import           Data.Char (ord)
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           WordList  (buildList)

-- addWord: (non thompsons) helper method for dictNFA
addWord :: [Char] -> AdjacencyList -> Set.Set Label -> State -> Set.Set State -> State -> (AdjacencyList, Set.Set Label, Set.Set State, State)
addWord (x:xs) adjList alph fromState finalStates toState = addWord xs newAdjList newAlph toState finalStates newToState where
  t = Label (ord x)
  newToState = toState + 1
  newAlph = Set.insert t alph
  newAdjList = Map.insertWith (++) fromState [(t, toState)] adjList
addWord [] adjList alph lastState finalStates toState = (adjList, alph, newFinals, toState) where
  newFinals = Set.insert lastState finalStates

-- dictNFA: (non thompsons) helper method for buildDictNFA
dictNFA :: [[Char]] -> AdjacencyList -> Set.Set Label -> State -> Set.Set State -> State -> (AdjacencyList, Set.Set Label, Set.Set State)
dictNFA (x:xs) lst alph st fi l = dictNFA xs nlst nalph st nfi nl where
  (nlst, nalph, nfi, nl) = addWord x lst alph 0 fi il
  il = l+1
dictNFA [] lst alph _ fi _ = (lst, alph, fi)

-- makeDictNFA: Build NFA from dict of accepted strings in a language
makeDictNFA :: [[Char]] -> Automaton
makeDictNFA l@(_:_) = Automaton lst alph start fi where
  alph = Epsilon : Set.toList salph
  (lst, salph, fi) = dictNFA l (Map.insert 0 [] Map.empty) Set.empty 0 Set.empty 0
  start = Set.singleton 0
makeDictNFA [] = error "empty dictionary"

dictNfa :: FilePath -> IO Automaton
dictNfa fp = do
    wordList <- buildList fp
    return $ makeDictNFA wordList
