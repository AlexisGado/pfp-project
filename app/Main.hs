module Main (main) where
import           Automaton          (Automaton (..), dumbAutomaton)
import qualified Data.Map           as Map
import qualified SubsetConstruction as SC

main :: IO ()
main = print $ Map.size adj
        where Automaton adj _ _ _ = SC.nfaToDfa $ dumbAutomaton 1000
