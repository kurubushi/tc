{-#LANGUAGE TupleSections#-}
{-#LANGUAGE FlexibleContexts#-}

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
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import Data.List (foldl')
import Control.Monad
import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Catch (Exception)
import Data.Semigroup

type Var = String

data EMaps = EMaps {
    mapAs   :: Map Var (Set Alphabet)
  , mapNd   :: Map Var (Nd.Nd (QD String) Set)
  , mapTdtt :: Map Var (Tdtt.Tdtt (QD String) Set)
}

instance Semigroup EMaps where
  m1 <> m2 = EMaps {
      mapAs = mapAs m1 `Map.union` mapAs m2
    , mapNd = mapNd m1 `Map.union` mapNd m2
    , mapTdtt = mapTdtt m1 `Map.union` mapTdtt m2
  }

emptyEMaps = EMaps Map.empty Map.empty Map.empty

data Expr = EAlphabet (Set Alphabet)
          | ENd (Nd.Nd (QD String) Set)
          | ETdtt (Tdtt.Tdtt (QD String) Set)

data Exec = Typecheck Var Var Var Var Var
  deriving (Eq, Ord)

instance Show Exec where
  show (Typecheck tdtt ind ias ond oas) =
    "typecheck! " ++ tdtt ++ " : " ++
    ind ++ "(" ++ ias ++ ") -> " ++ ond ++ "(" ++ oas ++ ")"

type Program = (EMaps,[Exec])

data ProgramError = ProgramError {reson :: String}
                  | NoVar {reson :: String}
  deriving Show

instance Exception ProgramError
instance Exception ParseError



parseAs :: String -> Parser a -> Parser a
parseAs tag p = sac *> string tag *> sac *> p

parseVar :: Parser a -> Parser (Var, a)
parseVar p = (,)
  <$> (sac *> var)
  <*> (sac *> string "=" *> p)

parseToSet :: Ord a => Parser a -> Parser (Set a)
parseToSet p = Set.fromList
  <$> (sac *> char '{'
       *> sac *> sepBy (p <* sac) (colon *> sac)
       <* sac <* char '}')

parseToSetAs :: Ord a => String -> Parser a -> Parser (Set a)
parseToSetAs tag = parseAs tag . parseToSet

var :: Parser Var
var = (:) <$> lower <*> many alphaNum


ndVar :: Parser (Var, Nd.Nd (QD String) Set)
ndVar = parseVar nd

-- Nd {states, states, states, rules}
-- let (Right myNd) = parse nd "" "Nd ( {q0,q1,q2}, {q2}, {q0}, {q2-> a (q0,q0), q2 -> b(q0,q0)})"
nd :: Parser (Nd.Nd (QD String) Set)
nd = parseAs "Bdta" $ Nd.Nd
  <$> (sac *> char '(' *> sac *> states) -- Q
  <*> (sac *> colon *> sac *> states) -- I
  <*> (sac *> colon *> sac *> states) -- F
  <*> (sac *> colon *> sac *> ndRules) -- Delta
  <*  sac <* char ')' <* sac

-- "ndr = NdRules {q0 -> a(q1,q2), q2 -> a(q2,q2)}"
ndRulesVar :: Parser (Var, Nd.Trans (QD String) Set)
ndRulesVar = parseVar (parseAs "BdtaRules" ndRules)
  
-- "{q0 -> a(q1,q2), q2 -> a(q2,q2)}"
ndRules :: Parser (Nd.Trans (QD String) Set)
ndRules = toTrans . mkTransFromSet <$> parseToSet ndRule
  where
  toTrans m q a = Map.findWithDefault Set.empty (q,a) m
  mkTransFromSet = Set.foldr combine Map.empty
  combine = Map.unionWith Set.union

-- "q -> a (q1, q2)"
ndRule :: Parser (Map (Nd.Expr (QD String), Alphabet) (Set (QD String)))
ndRule = makeRule
  <$> (sac *> var) -- q
  <*> (sac *> string "->" *> sac *> var) -- a
  <*> (sac *> char '(' *> sac *> var) -- q1
  <*> (sac *> colon *> sac *> var) -- q2
  <*  (sac *> char ')')
  <*  sac
  where
  makeRule q a q1 q2 = Map.singleton
    ((makeQ q1, makeQ q2), makeAlphabet a) (Set.singleton (makeQ q))


tdttVar :: Parser (Var, Tdtt.Tdtt (QD String) Set)
tdttVar = parseVar tdtt

-- Tdtt {states, states, rules}
-- let (Right myTdtt) = parse tdtt "" "Tdtt ({fib, aux}, {fib}, {fib(#) -> #, fib(a) -> a(fib(1),aux(1)), aux(#) -> #, aux(a) -> fib(1)})"
tdtt :: Parser (Tdtt.Tdtt (QD String) Set)
tdtt = parseAs "Tdtt" $ Tdtt.Tdtt
  <$> (sac *> char '(' *> sac *> states) -- P
  <*> (sac *> colon *> sac *> states) -- P0
  <*> (sac *> colon *> sac *> tdttRules) -- Pi
  <*  sac <* char ')' <* sac

-- "tdttr = TdttRules {p(a) -> b(p1(1),c(p2(2),#)), p(#) -> #}"
tdttRulesVar :: Parser (Var, Tdtt.Trans (QD String) Set)
tdttRulesVar = parseVar (parseAs "TdttRules" tdttRules)

-- "{p(a) -> b(p1(1),c(p2(2),#)), p(#) -> #}"
tdttRules :: Parser (Tdtt.Trans (QD String) Set)
tdttRules = toTrans . mkTransFromSet <$> parseToSet tdttRule
  where
  toTrans m q a = Map.findWithDefault Set.empty (q,a) m
  mkTransFromSet = Set.foldr combine Map.empty
  combine = Map.unionWith Set.union

-- p(a) -> b(p1(1),c(p2(2),#)) === p(a(x1,x2) -> b(p1(x1),c(p2(x2),#))
tdttRule :: Parser (Map (QD String, Alphabet) (Set (Tdtt.Expr (QD String))))
tdttRule = makeRule
  <$> (sac *> var) -- p
  <*> (sac *> char '(' *> sac *> tdttAlphabet) -- a
  <*  (sac *> char ')')
  <*> (sac *> string "->" *> sac *> tdttExpr) -- expr
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
  <$> (sac *> var) -- a
  <*> (sac *> char '(' *> sac *> tdttExpr) -- e1
  <*> (sac *> colon *> sac *> tdttExpr) -- e2
  <*  (sac *> char ')')
  <*  sac
  where
  makeExpr a e1 e2 = Tdtt.ExprA (makeAlphabet a) (e1,e2)

tdttExprL :: Parser (Tdtt.Expr (QD String))
tdttExprL = sac *> endAlphabetSt *> sac *> pure Tdtt.ExprL

tdttExprP :: Parser (Tdtt.Expr (QD String))
tdttExprP = makeExpr
  <$> (sac *> var) -- p
  <*> (sac *> char '(' *> sac *> number) -- 1
  <*  (sac *> char ')')
  where
  makeExpr p = Tdtt.ExprP (makeQ p)

colon :: Parser Char
colon = char ','

number :: Parser Int
number = read <$> many1 digit

endAlphabetSt :: Parser String
endAlphabetSt = string "#"

-- spaces and comments
sac :: Parser ()
sac = skipMany comments *> spaces

comments :: Parser ()
comments = try $
  spaces <* string "--" <* skipMany (noneOf "\n") <* char '\n'


-- "{q0, q1, q2}"
states :: Parser (Set (QD String))
states = Set.map makeQ <$> parseToSet var

-- "Alphabets {a, b}"
alphabets :: Parser (Set Alphabet)
alphabets = Set.map makeAlphabet <$> parseToSetAs "Alphabets" var


typecheck :: Parser Exec
typecheck = Typecheck
  <$> (sac *> string "typecheck!" *> sac *> var) -- tdtt
  <*> (sac *> char ':' *> sac *> var) -- inputNd
  <*> (sac *> char '(' *> sac *> var) -- inputAlphabet
  <*> (sac *> char ')' *> sac *> string "->" *> sac *> var) --outputNd
  <*> (sac *> char '(' *> sac *> var) -- outputAlphabet
  <*  (sac *> char ')')

program :: Parser Program
program = foldr union (emptyEMaps,[]) <$> many program'
  where
  union (m1,e1) (m2,e2) = (m1<>m2, e1++e2)

program' :: Parser Program
program' = choice . map try $
  [makeAlphabets <$> parseVar alphabets
  ,makeNd        <$> ndVar
  ,makeTdtt      <$> tdttVar
  ,makeTypecheck <$> typecheck]
  where
  makeAlphabets as = (emptyEMaps {mapAs= makeMap as},[])
  makeNd nd        = (emptyEMaps {mapNd= makeMap nd},[])
  makeTdtt tdtt    = (emptyEMaps {mapTdtt= makeMap tdtt},[])
  makeTypecheck tc = (emptyEMaps, [tc])
  makeMap = uncurry Map.singleton

parseProgram :: String -> Either ParseError Program
parseProgram = parse program ""

lookupE :: (Show k, Ord k, MonadError ProgramError m) =>
  k -> Map k v -> m v
lookupE k m = maybe
  (throwError . NoVar $ "no such var: " ++ show k)
  return $ k `Map.lookup` m

execTest :: MonadError ProgramError m => EMaps -> Exec -> m (Int, Maybe (BTree Alphabet))
execTest maps ch@(Typecheck tdttv indv iav ondv oav) = 
  Tdtt.testTypeCheck
    <$> iav `lookupE` mapAs maps
    <*> oav `lookupE` mapAs maps
    <*> indv `lookupE` mapNd maps
    <*> ondv `lookupE` mapNd maps
    <*> tdttv `lookupE` mapTdtt maps
