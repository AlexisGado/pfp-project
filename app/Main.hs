module Main (main) where
import           Automaton          (Automaton (..))
import qualified Data.Map           as Map
import           RandomNfa          (ioRandomNfa)
import qualified SubsetConstruction as SC

usedAutomaton :: IO Automaton
-- random NFA with control over density and size
usedAutomaton = ioRandomNfa 600 20 10 50

-- NFA from a list of words
-- usedAutomaton = dictNfa "assets/words.txt"

-- NFA from a regular expression
-- usedAutomaton = return $ makeThompsonNFA $ P.parse regexParser "" "a*(a|b)b*"

-- Some simple examples
-- usedAutomaton = return exampleAutomaton
-- usedAutomaton = return exampleAutomaton2

-- a simple linear automaton to test scale
-- usedAutomaton = ioDumbAutomaton 300


main :: IO ()
main = do
    nfa <- usedAutomaton
    let Automaton nfaAdj _ _ _ = nfa
    let Automaton dfaAdj _ _ _ = SC.nfaToDfa nfa
    print $ Map.size nfaAdj
    print $ Map.size dfaAdj
