{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TupleSections #-}

module Tt.Tdtt.Utils where

import Atom.Types
import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Set.Utils as S
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

inf :: (StateSet s, Ord p, Ord q) => Nd q s -> Expr p -> q -> Ata.Expr (Q (p,q))
inf nd ExprL q
  | q `S.member` Nd.getFs nd = Ata.ExprTop
  | otherwise = Ata.ExprBottom
inf nd (ExprA a (e1,e2)) q =
  maybe Ata.ExprBottom (Ata.foldrExprOrWith infAndinf)
  . Map.lookup (q, a)
  . Nd.getTrans $ nd
  where
    infAndinf (Nd.Expr (q1,q2)) = inf nd e1 q1 `Ata.ExprAnd` inf nd e2 q2
inf nd (ExprP p n) q = Ata.ExprCond n (makeQ (p,q))


infer :: (StateSet s, Ord p, Ord q) => s Alphabet -> Tdtt (Q p) s -> Nd (Q q) s -> Ata (Q (Q p,Q q)) s
infer as tdtt nd = Ata {
    Ata.getQs = S.map conv qs
  , Ata.getIs = S.map conv is
  , Ata.getFs = S.map conv
      . S.fromList -- using List becase s (Expr a q) is not Ord.
      . map (\(q,((p,_),_)) -> (p,q))
      . filter (\(q,(_,es)) -> Ata.isTop
          . Ata.foldrExprOrWith (\e -> inf nd e q) $ es)
      . S.cartesian (S.toList . Nd.getQs $ ndc) -- :: [(q, ((p,a), s (Expr a p)))]
      . Map.toList -- :: [((p,a), s (Expr a p))]
      . Map.filterWithKey (\(_,a) _ -> isEnd a)
      . Tdtt.getTrans $ tdtt
  , Ata.getTrans = S.toMap
      . S.map (\((p,q),a) -> ((conv (p,q),a),)
          . Ata.foldrExprOrWith (Ata.foldrExprOrWith (\e -> inf nd e q))
          . Map.filterWithKey (\(p',a') _ -> isNotEnd a' && p'==p && a'==a)
          . Tdtt.getTrans $ tdtt)
      . S.cartesian qs $ as
}
  where
    conv = makeQ
    qs = S.cartesian (getPs tdtt) (Nd.getQs ndc)
    is = S.cartesian (getP0 tdtt) (Nd.getIs ndc)
    ndc = Nd.complement as nd


typecheck ::
  (Eq (s (Q (q1, Q (s (Q (Q p, Q q2)))))), Ord p, Ord q2, Ord q1, Ord (s (Q (Q p, Q q2))), StateSet s) =>
  s Alphabet -> Nd q1 s -> Nd (Q q2) s -> Tdtt (Q p) s -> Bool
typecheck as inputNd outputNd tdtt = Nd.isEmpty testNd
  where
    inferAta = infer as tdtt outputNd
    inferNd = Ata.toNd as inferAta
    testNd = inputNd `Nd.intersection` inferNd
