{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TupleSections #-}

module Tt.Tdtt.Utils where

import Atom.Types
import Set.Types (StateSet)
import qualified Set.Types as S
import Tt.Tdtt.Types
import qualified Tt.Tdtt.Types as Tdtt
import qualified Data.Map as Map
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import Ta.Nd.Types (Nd(..))
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import Data.Maybe (fromMaybe)

inf :: (Alphabet a, Q q, Q p, StateSet s) =>
  Nd a q s -> Expr a p -> q -> Ata.Expr (p,q)
inf nd ExprL q
  | q `S.member` Nd.getFs nd = Ata.ExprTop
  | otherwise = Ata.ExprBottom
inf nd (ExprA a (e1,e2)) q =
  maybe Ata.ExprBottom (S.foldr aux Ata.ExprBottom) 
  . Map.lookup (q, a)
  . Nd.getTrans $ nd
  where
    aux (Nd.Expr (q1,q2)) acc = 
      (inf nd e1 q1 `Ata.ExprAnd` inf nd e2 q2) `Ata.ExprOr` acc
inf nd (ExprP p n) q = Ata.ExprCond n (p,q)


infer :: (Alphabet a, Q q, Q p, StateSet s) =>
  s a -> Tdtt a p s -> Nd a q s -> Ata a (p,q) s
infer as tdtt nd = Ata {
    Ata.getQs = qs
  , Ata.getIs = S.cartesian (getP0 tdtt) (Nd.getIs ndc)
  , Ata.getFs = S.fromList
      . map (\(((p,a),_),q) -> (p,q))
      . filter (\((_,es),q) -> Ata.isTop
          . S.foldr (\e acc -> inf nd e q `Ata.ExprOr` acc) Ata.ExprBottom $ es)
      . flip cartesian (S.toList . Nd.getQs $ ndc) -- :: [(((p,a), s (Expr a p)) , q)]
      . Map.assocs -- :: [((p,a), s (Expr a p))]
      . Map.filterWithKey (\(_,a) _ -> a==end)
      . Tdtt.getTrans $ tdtt
  , Ata.getTrans = Map.fromList
      . S.toList
      . S.map (\((p,q),a) -> (((p,q),a),)
          . Map.foldr (\es acc ->
              (S.foldr (\e acc ->
                inf nd e q `Ata.ExprOr` acc) Ata.ExprBottom es) `Ata.ExprOr` acc) Ata.ExprBottom
          . Map.filterWithKey (\(p',a') _ -> p'==p && a'==a && a'/=end)
          . Tdtt.getTrans $ tdtt)
      . S.cartesian qs $ as
}
  where
    ndc = Nd.complement nd
    qs = S.cartesian (getPs tdtt) (Nd.getQs ndc)
    cartesian xs ys = [(x, y) | x <- xs, y <- ys]
