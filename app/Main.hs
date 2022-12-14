module Main (main) where
import           Automaton          (exampleAutomaton2)
import qualified SubsetConstruction as SC

main :: IO ()
main = print $ SC.nfaToDfa exampleAutomaton2
