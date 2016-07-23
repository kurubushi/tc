module Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.String
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Atom.Types
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import qualified Tt.Tdtt.Types as Tdtt
import qualified Tt.Tdtt.Utils as Tdtt
import Control.Monad
import Control.Applicative ((<$>), (*>), (<*), (<*>))

type Var = String

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

-- "ndr = NdRules {q0 -> a(q1,q2), q2 -> a(q2,q2)}"
ndRulesVar :: Parser (Var, Nd.Trans (QD String) Set)
ndRulesVar = parseVar ndRules
  
-- "NdRules {q0 -> a(q1,q2), q2 -> a(q2,q2)}"
ndRules :: Parser (Nd.Trans (QD String) Set)
ndRules = mkTransFromSet <$> parseToSetAs "NdRules" ndRule
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

-- "tdttr = TdttRules {p(a) -> b(p1(1),c(p2(2),#)), p(#) -> #}"
tdttRulesVar :: Parser (Var, Tdtt.Trans (QD String) Set)
tdttRulesVar = parseVar tdttRules

-- "TdttRules {p(a) -> b(p1(1),c(p2(2),#)), p(#) -> #}"
tdttRules :: Parser (Tdtt.Trans (QD String) Set)
tdttRules = mkTransFromSet <$> parseToSetAs "TdttRules" tdttRule
  where
  mkTransFromSet = Set.foldr combine Map.empty
  combine = Map.unionWith Set.union

-- p(a) -> b(p1(1),c(p2(2),#)) === p(a(x1,x2) -> b(p1(x1),c(p2(x2),#))
tdttRule :: Parser (Tdtt.Trans (QD String) Set)
tdttRule = makeRule
  <$> (spaces *> var) -- p
  <*> (spaces *> char '(' *> spaces *> (var <|> endAlphabetSt)) -- a --!!!
  <*  (spaces *> char ')')
  <*> (spaces *> string "->" *> spaces *> tdttExpr) -- expr
  where
  makeRule p a expr = Map.singleton
    (makeQ p, makeAlphabet a) (Set.singleton expr)

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


-- "States {q0, q1, q2}"
states :: Parser (Set (QD String))
states = Set.map makeQ <$> parseToSetAs "States" var

-- "Alphabets {a, b}"
alphabets :: Parser (Set Alphabet)
alphabets = Set.map makeAlphabet <$> parseToSetAs "Alphabets" var
