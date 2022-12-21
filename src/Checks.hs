module Checks (checkAccept, checkAlphabet) where
import           Automaton (Automaton (..), Label (..), State)
import           Data.Char (ord)
import           Data.List (find)
import qualified Data.Map  as Map

-- checkAlphabet: Check if word agrees with alphabet for a given Automaton
checkAlphabet :: [Char] -> Automaton -> Bool
checkAlphabet (x:xs) dfa@(Automaton _ alph _ _) = n && checkAlphabet xs dfa
        where n = Label (ord x) `elem` alph
checkAlphabet [] _ = True

-- checkAccept: check if word is in a language. Automaton MUST be a DFA
checkAccept :: [Char] -> Automaton -> State -> Bool
checkAccept (x:xs) dfa c = case findTransition x dfa c of
  Just e  -> checkAccept xs dfa n where (_,n) = e
  Nothing -> False
checkAccept [] (Automaton _ _ _ end) c = c `elem` end

findTransition :: Char -> Automaton -> Int -> Maybe (Label, State)
findTransition x (Automaton lst _ _ _) c = case Map.lookup c lst of
 Just es -> find (\y -> fst y == Label (ord x)) es
 Nothing -> Nothing
