{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tt.Tdtt.Utils where

import Atom.Types
import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Set.Utils as S
import Tt.Tdtt.Types
import qualified Tt.Tdtt.Types as Tdtt
import qualified Data.Map.Lazy as Map
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import Ta.Nd.Types (Nd(..))
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import Data.Maybe (fromMaybe)
import Data.Set (Set)

inf :: (StateSet s, Q p, Q q, Q q', QElem q' ~ (p,q)) =>
  Nd q s -> Expr p -> q -> Ata.Expr q'
inf nd ExprL q
  | q `S.member` Nd.getFs nd = Ata.ExprTop
  | otherwise = Ata.ExprBottom
inf nd (ExprA a (e1,e2)) q =
  Ata.foldrExprOrWith infAndinf $ findExpr (q, a)
  where
    findExpr (q, a)
      | isEnd a = S.empty
      | otherwise = S.filter (\e -> q `S.member` (Nd.getTrans nd) e a)
                      $ Nd.getQs nd `S.cartesian` Nd.getQs nd
    infAndinf (q1,q2) = inf nd e1 q1 `Ata.ExprAnd` inf nd e2 q2
inf nd (ExprP p n) q = Ata.ExprCond n (makeQ (p,q))


infer :: forall s p q q'. (StateSet s, Q p, Q q, Q q', QElem q' ~ (p,q)) =>
  s Alphabet -> s Alphabet -> Tdtt p s -> Nd q s -> Ata q' s
infer inputAs outputAs tdtt nd = Ata {
    Ata.getQs = S.map conv qs
  , Ata.getIs = S.map conv is
  , Ata.getFs =
      S.map conv
      . S.filter (isTop . makeExprCond)
      $ Tdtt.getPs tdtt `S.cartesian` Nd.getQs ndc
  , Ata.getTrans = \q' a ->
      fromMaybe Ata.ExprBottom 
        $ makeTransExpr a <$> getQ q'
}
  where
    conv = makeQ
    qs = S.cartesian (getPs tdtt) (Nd.getQs ndc)
    is = S.cartesian (getP0 tdtt) (Nd.getIs ndc)
    ndc = Nd.complement outputAs nd
    makeExprCond (p,q) = Ata.foldrExprOrWith (\e -> inf ndc e q)
      $ (Tdtt.getTrans tdtt) p endAlphabet
    isTop = Ata.isTop :: Ata.Expr q' -> Bool
    makeTransExpr a (p,q) = Ata.foldrExprOrWith (\e -> inf ndc e q)
      $ (Tdtt.getTrans tdtt) p a


typecheck :: forall p q1 q2 s q3 q4 q5.
  (Q p, Q q1, Q q2, StateSet s,
   Q q3, q3 ~ QD (p,q2),
   Q q4, q4 ~ QD (s q3),
   Q q5, q5 ~ QD (q1,q4),
   Ord (s q3), Eq (s q5)) =>
  s Alphabet -> s Alphabet -> Nd q1 s -> Nd q2 s -> Tdtt p s -> Bool
typecheck inputAs outputAs inputNd outputNd tdtt = Nd.isEmpty outputAs testNd
  where
    inferAta = infer inputAs outputAs tdtt outputNd :: Ata q3 s
    inferNd = Ata.toNd inputAs inferAta :: Nd q4 s
    testNd = inputNd `Nd.intersection` inferNd :: Nd q5 s


testTypeCheck :: forall p q1 q2 q3 q4 q5.
  (Q p, Q q1, Q q2,
   q3 ~ QD (p,q2),
   q4 ~ QD (Set q3),
   q5 ~ QD (q1,q4)) =>
   Set Alphabet -> Set Alphabet -> Nd q1 Set -> Nd q2 Set -> Tdtt p Set -> (Int, Maybe (BTree Alphabet))
testTypeCheck inputAs outputAs inputNd outputNd tdtt
  | S.null qs = (testNdSize, Nothing)
  | otherwise = (testNdSize, Nd.sampleCounterExample testNd memo (head . S.toList $ qs))
  where
  inferAta = infer inputAs outputAs tdtt outputNd :: Ata q3 Set
  inferNd = Ata.toNd inputAs inferAta :: Nd q4 Set
  testNd = inputNd `Nd.intersection` inferNd :: Nd q5 Set
  testNdSize = S.size $ Nd.getQs testNd
  (memo, qs) = Nd.isEmptyWithQneFollow testNd (Nd.getFs testNd) Map.empty
