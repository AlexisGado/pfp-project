module Dictionary (makeDictNFA) where

import           Automaton (AdjacencyList, Automaton (..), Label (..), State)
import           Data.Char (ord)
import qualified Data.Map  as Map
import qualified Data.Set  as Set

-- addWord: (non thompsons) helper method for dictNFA
addWord :: [Char] -> AdjacencyList -> Set.Set Label -> State -> Set.Set State -> State -> (AdjacencyList, Set.Set Label, Set.Set State, State)
addWord (x:xs) adjList alph st finalStates l = addWord xs newAdjList newAlph nl finalStates nl where
  t = Label (ord x)
  nl = l + 1
  newAlph = Set.insert t alph
  newAdjList = Map.insert st [(t, nl)] adjList
addWord [] adjList alph _ finalStates l = (adjList, alph, newFinals, l) where
  newFinals = Set.insert l finalStates

-- dictNFA: (non thompsons) helper method for buildDictNFA
dictNFA :: [[Char]] -> AdjacencyList -> Set.Set Label -> State -> Set.Set State -> State -> (AdjacencyList, Set.Set Label, Set.Set State)
dictNFA (x:xs) lst alph st fi l = dictNFA xs nlst nalph st nfi nl where
  (nlst, nalph, nfi, nl) = addWord x ilst alph il fi il
  tmp = case Map.lookup 0 lst of
    Just jTmp -> jTmp
    Nothing   -> error "No initial node found"
  newstart = (Epsilon, il):tmp
  ilst = Map.insert 0 newstart lst
  il = l+1
dictNFA [] lst alph _ fi _ = (lst, alph, fi)

-- makeDictNFA: Build NFA from dict of accepted strings in a language
makeDictNFA :: [[Char]] -> Automaton
makeDictNFA l@(_:_) = Automaton lst alph start fi where
  alph = Epsilon : Set.toList salph
  (lst, salph, fi) = dictNFA l (Map.insert 0 [] Map.empty) Set.empty 0 Set.empty 0
  start = Set.singleton 0
makeDictNFA [] = error "empty dictionary"
