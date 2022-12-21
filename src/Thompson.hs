module Thompson (regexParser, makeThompsonNFA, checkAccept, checkAlphabet, buildList) where
import           Automaton                          (Automaton (..), Label (..),
                                                     State)
import           Control.Monad                      (msum)
import           Data.Char                          (isAlpha, ord)
import           Data.List                          (find)
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import qualified Data.Text                          as T
import           Data.Text.IO                       as TIO (readFile)
import qualified Text.ParserCombinators.Parsec      as P
import qualified Text.ParserCombinators.Parsec.Expr as PE

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

-- Build NFA from regex AST following Thompson's Algorithm
makeThompsonNFA :: Either a Node -> Automaton
makeThompsonNFA ((Right ast)) = Automaton table alphabet start end where
  (table, _) = thompsons ast 1 0 1
  alphabet = Set.toList (buildAlph ast Set.empty)
  start = Set.singleton 1
  end = Set.singleton 0
makeThompsonNFA (Left _) = error "Bad AST"

-- buildAlph: Build alphabet from regex AST. Helper method for makeThompsonNFA
buildAlph :: Node -> Set.Set Label -> Set.Set Label
buildAlph (Concat r l) alph  = Set.union (buildAlph r alph) (buildAlph l alph)
buildAlph (Star l) alph      = buildAlph l alph
buildAlph (Or r l) alph      = Set.union (buildAlph r alph) (buildAlph l alph)
buildAlph (Character x) alph = Set.insert (Label x) alph

-- thompsons: Build adjacency list from regex AST. Helper method for makeThompsonNFA
thompsons :: (Num b, Ord b) => Node -> b -> b -> b -> (Map.Map b [(Label, b)], b)
thompsons (Character x) q f l = (Map.fromList [(q, [(Label x, f)])], l)
thompsons (Concat s t) q f l = (Map.union smap tmap, lt) where
  (smap, ls) = thompsons s q i ln
  (tmap, lt) = thompsons t i f ls
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

buildList :: FilePath -> IO [[Char]]
buildList filename = do
 h <- TIO.readFile filename
 let l = T.words h
 let lnorm = map normalize l
 let lnormset = Set.fromList lnorm
 let lnormunique = Set.toList lnormset
 return lnormunique

normalize :: T.Text -> [Char]
normalize string = [ x | x <- a, isAlpha x ] where a = T.unpack (T.toLower string)
