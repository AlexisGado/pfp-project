module Thompson2 (regexParser, makeNFA) where
import           Control.Monad                      (msum)
import           Data.Char                          (ord)
import qualified Text.ParserCombinators.Parsec      as P
import qualified Text.ParserCombinators.Parsec.Expr as PE
import qualified Data.Set                           as Set
import            Automaton                           (Automaton(..), Label(..), AdjacencyList)
import qualified Data.Map as Map

data Node = Concat !Node !Node | Star !Node | Or !Node !Node | Character !Int deriving(Show)

regexParser :: P.Parser Node
regexParser = PE.buildExpressionParser opTable base
  where
    opTable = [   [ PE.Postfix (P.char '*' >> return Star)]
                , [ PE.Infix (return Concat) PE.AssocLeft]
                , [ PE.Infix (P.char '|' >> return Or) PE.AssocLeft]
              ]
    base = msum [Character . ord <$> P.noneOf "()*|", parens regexParser]
    parens = P.between (P.char '(') (P.char ')')





makeNFA :: Either a Node -> Automaton
makeNFA ((Right ast)) = Automaton table alphabet start end where 
  (table, _) = thompsons ast 1 0 1
  alphabet = Set.toList ((buildAlph ast Set.empty))
  start = Set.singleton 1
  end = Set.singleton 0
makeNFA (Left _) = error "Bad AST" 



buildAlph :: Node -> Set.Set Label -> Set.Set Label
buildAlph (Concat r l) alph = Set.union (buildAlph r alph) (buildAlph l alph)
buildAlph (Star l) alph = buildAlph l alph
buildAlph (Or r l) alph = Set.union (buildAlph r alph) (buildAlph l alph)
buildAlph (Character x) alph = Set.insert (Label x) alph 


thompsons :: (Num b, Ord b) => Node -> b -> b -> b -> (Map.Map b [(Label, b)], b)
thompsons (Character x) q f l = (Map.fromList [(q, [(Label x, f)])], l)
thompsons (Concat s t) q f l = ((Map.union smap tmap), lt) where
  (smap, ls) = (thompsons s q i ln)
  (tmap, lt) = (thompsons t i f ls)
  i = l + 1
  ln = l + 1
thompsons (Or s t) q f l = (Map.union (Map.union smap tmap) omap, lt) where
  omap = Map.fromList [(q, [(Epsilon, si), (Epsilon, ti)]), (sf, [(Epsilon, f)]), (tf, [(Epsilon, f)])]
  (smap, ls) = thompsons s si sf ln
  (tmap, lt) = thompsons t ti tf ls
  si = l+1; sf = l+2
  ti = l+3; tf = l+4
  ln = l+4
thompsons (Star s) q f l = (Map.union smap stmap, ls) where
  stmap = Map.fromList [(q, [(Epsilon, si), (Epsilon, f)]), (sf, [(Epsilon, si), (Epsilon, f)])]
  (smap, ls) = thompsons s si sf ln
  si = l+1; sf = l+2
  ln = l+2

--showNode :: Either a Node -> String
--showNode (Right (Concat x y)) = show "hi"