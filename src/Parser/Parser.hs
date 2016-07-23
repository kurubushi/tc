{-#LANGUAGE TupleSections#-}

module Parser.Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.String
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Atom.Types
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import qualified Tt.Tdtt.Types as Tdtt
import qualified Tt.Tdtt.Utils as Tdtt
import Data.List (foldl')
import Control.Monad
import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Control.Arrow

type Var = String

type EMap = Map Var Expr

data Expr = EAlphabet (Set Alphabet)
          | ENd (Nd.Nd (QD String) Set)
          | ETdtt (Tdtt.Tdtt (QD String) Set)

data Exec = Typecheck Var Var Var Var Var
  deriving (Eq, Ord, Show)

type Program = (EMap,[Exec])



parseAs :: String -> Parser a -> Parser a
parseAs tag p = spaces *> string tag *> spaces *> p

parseVar :: Parser a -> Parser (Var, a)
parseVar p = (,)
  <$> (spaces *> var)
  <*> (spaces *> string "=" *> p)

parseToSet :: Ord a => Parser a -> Parser (Set a)
parseToSet p = Set.fromList
  <$> (spaces *> char '{'
       *> spaces *> sepBy (p <* spaces) (colon *> spaces)
       <* spaces <* char '}')

parseToSetAs :: Ord a => String -> Parser a -> Parser (Set a)
parseToSetAs tag = parseAs tag . parseToSet

var :: Parser Var
var = (:) <$> lower <*> many alphaNum


ndVar :: Parser (Var, Nd.Nd (QD String) Set)
ndVar = parseVar nd

-- Nd {states, states, states, rules}
-- let (Right myNd) = parse nd "" "Nd ( {q0,q1,q2}, {q2}, {q0}, {q2-> a (q0,q0), q2 -> b(q0,q0)})"
nd :: Parser (Nd.Nd (QD String) Set)
nd = parseAs "Nd" $ Nd.Nd
  <$> (spaces *> char '(' *> spaces *> states) -- Q
  <*> (spaces *> colon *> spaces *> states) -- I
  <*> (spaces *> colon *> spaces *> states) -- F
  <*> (spaces *> colon *> spaces *> ndRules) -- Delta
  <*  spaces <* char ')' <* spaces

-- "ndr = NdRules {q0 -> a(q1,q2), q2 -> a(q2,q2)}"
ndRulesVar :: Parser (Var, Nd.Trans (QD String) Set)
ndRulesVar = parseVar (parseAs "NdRules" ndRules)
  
-- "{q0 -> a(q1,q2), q2 -> a(q2,q2)}"
ndRules :: Parser (Nd.Trans (QD String) Set)
ndRules = mkTransFromSet <$> parseToSet ndRule
  where
  mkTransFromSet = Set.foldr combine Map.empty
  combine = Map.unionWith Set.union

-- "q -> a (q1, q2)"
ndRule :: Parser (Nd.Trans (QD String) Set)
ndRule = makeRule
  <$> (spaces *> var) -- q
  <*> (spaces *> string "->" *> spaces *> var) -- a
  <*> (spaces *> char '(' *> spaces *> var) -- q1
  <*> (spaces *> colon *> spaces *> var) -- q2
  <*  (spaces *> char ')')
  <*  spaces
  where
  makeRule q a q1 q2 = Map.singleton
    (makeQ q, makeAlphabet a) (Set.singleton (Nd.Expr (makeQ q1, makeQ q2)))


tdttVar :: Parser (Var, Tdtt.Tdtt (QD String) Set)
tdttVar = parseVar tdtt

-- Tdtt {states, states, rules}
-- let (Right myTdtt) = parse tdtt "" "Tdtt ({fib, aux}, {fib}, {fib(#) -> #, fib(a) -> a(fib(1),aux(1)), aux(#) -> #, aux(a) -> fib(1)})"
tdtt :: Parser (Tdtt.Tdtt (QD String) Set)
tdtt = parseAs "Tdtt" $ Tdtt.Tdtt
  <$> (spaces *> char '(' *> spaces *> states) -- P
  <*> (spaces *> colon *> spaces *> states) -- P0
  <*> (spaces *> colon *> spaces *> tdttRules) -- Pi
  <*  spaces <* char ')' <* spaces

-- "tdttr = TdttRules {p(a) -> b(p1(1),c(p2(2),#)), p(#) -> #}"
tdttRulesVar :: Parser (Var, Tdtt.Trans (QD String) Set)
tdttRulesVar = parseVar (parseAs "TdttRules" tdttRules)

-- "{p(a) -> b(p1(1),c(p2(2),#)), p(#) -> #}"
tdttRules :: Parser (Tdtt.Trans (QD String) Set)
tdttRules = mkTransFromSet <$> parseToSet tdttRule
  where
  mkTransFromSet = Set.foldr combine Map.empty
  combine = Map.unionWith Set.union

-- p(a) -> b(p1(1),c(p2(2),#)) === p(a(x1,x2) -> b(p1(x1),c(p2(x2),#))
tdttRule :: Parser (Tdtt.Trans (QD String) Set)
tdttRule = makeRule
  <$> (spaces *> var) -- p
  <*> (spaces *> char '(' *> spaces *> tdttAlphabet) -- a
  <*  (spaces *> char ')')
  <*> (spaces *> string "->" *> spaces *> tdttExpr) -- expr
  where
  makeRule p a expr = Map.singleton
    (makeQ p, a) (Set.singleton expr)

tdttAlphabet :: Parser Alphabet
tdttAlphabet = toAlphabet <$> (endAlphabetSt <|> var)
  where
  toAlphabet "#" = endAlphabet
  toAlphabet a   = makeAlphabet a

tdttExpr :: Parser (Tdtt.Expr (QD String))
tdttExpr = choice . map try $ [tdttExprA, tdttExprL, tdttExprP]
  -- it must use try

tdttExprA :: Parser (Tdtt.Expr (QD String))
tdttExprA = makeExpr
  <$> (spaces *> var) -- a
  <*> (spaces *> char '(' *> spaces *> tdttExpr) -- e1
  <*> (spaces *> colon *> spaces *> tdttExpr) -- e2
  <*  (spaces *> char ')')
  <*  spaces
  where
  makeExpr a e1 e2 = Tdtt.ExprA (makeAlphabet a) (e1,e2)

tdttExprL :: Parser (Tdtt.Expr (QD String))
tdttExprL = spaces *> endAlphabetSt *> spaces *> pure Tdtt.ExprL

tdttExprP :: Parser (Tdtt.Expr (QD String))
tdttExprP = makeExpr
  <$> (spaces *> var) -- p
  <*> (spaces *> char '(' *> spaces *> number) -- 1
  <*  (spaces *> char ')')
  where
  makeExpr p = Tdtt.ExprP (makeQ p)

colon :: Parser Char
colon = char ','

number :: Parser Int
number = read <$> many1 digit

endAlphabetSt :: Parser String
endAlphabetSt = string "#"


-- "{q0, q1, q2}"
states :: Parser (Set (QD String))
states = Set.map makeQ <$> parseToSet var

-- "Alphabets {a, b}"
alphabets :: Parser (Set Alphabet)
alphabets = Set.map makeAlphabet <$> parseToSetAs "Alphabets" var


typecheck :: Parser Exec
typecheck = Typecheck
  <$> (spaces *> string "typecheck!" *> spaces *> var) -- inputAlphabet
  <*> (spaces *> var) -- outputAlphabet
  <*> (spaces *> var) -- inputNd
  <*> (spaces *> var) -- outputNd
  <*> (spaces *> var) -- tdtt

program :: Parser Program
program = foldr union (Map.empty,[]) <$> many program'
  where
  union (m1,e1) (m2,e2) = (m1`Map.union`m2, e1++e2)

program' :: Parser Program
program' = choice . map try $
  [((,[]) . makeMap . second EAlphabet) <$> parseVar alphabets
  ,((,[]) . makeMap . second ENd)       <$> ndVar
  ,((,[]) . makeMap . second ETdtt)     <$> tdttVar
  ,((Map.empty,) . makeList)            <$> typecheck]
  where
  makeMap = uncurry Map.singleton
  makeList = pure

parseProgram :: String -> Either ParseError Program
parseProgram = parse program ""
