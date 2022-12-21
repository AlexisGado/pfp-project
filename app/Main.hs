module Main (main) where
import           Automaton          (Automaton (..), dumbAutomaton)
import qualified Data.Map           as Map
import           Dictionary         (makeDictNFA)
import qualified SubsetConstruction as SC
import           Thompson           (buildList)

nfa1 :: IO Automaton
nfa1 = return $ dumbAutomaton 500

dictNfa :: IO Automaton
dictNfa = do
    wordList <- buildList "assets/words.txt"
    return $ makeDictNFA wordList

main :: IO ()
main = do
    nfa <- dictNfa
    let Automaton nfaAdj alph1 _ _ = nfa
    let Automaton dfaAdj alph2 _ _ = SC.nfaToDfa nfa
    print alph1
    print alph2
    print $ Map.size nfaAdj
    print $ Map.size dfaAdj
