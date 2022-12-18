module Thompson (regexParser, makeNFA, buildDictNFA) where
import           Automaton                          (Automaton (..), Label (..))
import           Control.Monad                      (msum)
import           Data.Char                          (ord)
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
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


makeNFA :: Either a Node -> Automaton
makeNFA ((Right ast)) = Automaton table alphabet start end where
  (table, _) = thompsons ast 1 0 1
  alphabet = Set.toList (buildAlph ast Set.empty)
  start = Set.singleton 1
  end = Set.singleton 0
makeNFA (Left _) = error "Bad AST"

buildAlph :: Node -> Set.Set Label -> Set.Set Label
buildAlph (Concat r l) alph  = Set.union (buildAlph r alph) (buildAlph l alph)
buildAlph (Star l) alph      = buildAlph l alph
buildAlph (Or r l) alph      = Set.union (buildAlph r alph) (buildAlph l alph)
buildAlph (Character x) alph = Set.insert (Label x) alph


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


addWord :: (Num a, Ord a) => [Char] -> Map.Map a [(Label, a)] -> Set.Set Label -> a -> Set.Set a -> a -> (Map.Map a [(Label, a)], Set.Set Label, Set.Set a, a)
addWord (x:xs) lst alph st fi l = addWord xs nlst nalph nl fi nl where
  t = Label (ord x)
  nl = l + 1
  nalph = Set.insert t alph
  nlst = Map.insert st [(t, nl)] lst
addWord [] lst alph _ fi l = (lst, alph, nfi, l) where
  nfi = Set.insert l fi

dictNFA :: (Num t, Ord t) => [[Char]] -> Map.Map t [(Label, t)] -> Set.Set Label -> t -> Set.Set t -> t -> (Map.Map t [(Label, t)], Set.Set Label, Set.Set t)
dictNFA (x:xs) lst alph st fi l = dictNFA xs nlst nalph st nfi nl where
  (nlst, nalph, nfi, nl) = addWord x ilst alph il fi il
  Just tmp = Map.lookup 0 lst
  newstart = (Epsilon, il):tmp
  ilst = Map.insert 0 newstart lst
  il = l+1
dictNFA [] lst alph _ fi _ = (lst, alph, fi)

buildDictNFA :: [[Char]] -> Automaton
buildDictNFA l@(x:xs) = Automaton lst alph start fi where
  alph = Set.toList salph
  (lst, salph, fi) = dictNFA l (Map.insert 0 [] Map.empty) Set.empty 0 Set.empty 0
  start = Set.singleton 0 
buildDictNFA [] = error "empty dictionary"

