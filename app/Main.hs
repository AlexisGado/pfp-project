module Main (main) where
import           Automaton          (Automaton (..))
import qualified Data.Map           as Map
import           RandomNfa          (randomNFA)
import qualified SubsetConstruction as SC

-- nfa1 :: IO Automaton
-- nfa1 = return $ dumbAutomaton 500

rdNfa :: IO Automaton
rdNfa = return $ randomNFA 600 20 10 50

-- dictNfa :: IO Automaton
-- dictNfa = do
--     wordList <- buildList "assets/words.txt"
--     return $ makeDictNFA wordList

main :: IO ()
main = do
    nfa <- rdNfa
    let Automaton nfaAdj _ _ _ = nfa
    let Automaton dfaAdj _ _ _ = SC.nfaToDfa nfa
    print $ Map.size nfaAdj
    print $ Map.size dfaAdj
