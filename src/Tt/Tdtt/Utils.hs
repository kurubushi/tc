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

inf :: StateSet s => Nd s -> Expr -> Q -> Ata.Expr
inf = undefined
--inf nd ExprL q
--  | q `S.member` Nd.getFs nd = Ata.ExprTop
--  | otherwise = Ata.ExprBottom
--inf nd (ExprA a (e1,e2)) q =
--  maybe Ata.ExprBottom (Ata.foldrExprOrWith infAndinf)
--  . Map.lookup (q, a)
--  . Nd.getTrans $ nd
--  where
--    infAndinf (Nd.Expr (q1,q2)) = inf nd e1 q1 `Ata.ExprAnd` inf nd e2 q2
--inf nd (ExprP p n) q = Ata.ExprCond n (p,q)


infer :: StateSet s => s Alphabet -> Tdtt s -> Nd s -> Ata s
infer = undefined
--infer as tdtt nd = Ata {
--    Ata.getQs = qs
--  , Ata.getIs = S.cartesian (getP0 tdtt) (Nd.getIs ndc)
--  , Ata.getFs = S.fromList -- using List becase s (Expr a q) is not Ord.
--      . map (\(q,((p,_),_)) -> (p,q))
--      . filter (\(q,(_,es)) -> Ata.isTop
--          . Ata.foldrExprOrWith (\e -> inf nd e q) $ es)
--      . S.cartesian (S.toList . Nd.getQs $ ndc) -- :: [(q, ((p,a), s (Expr a p)))]
--      . Map.toList -- :: [((p,a), s (Expr a p))]
--      . Map.filterWithKey (\(_,a) _ -> a==end)
--      . Tdtt.getTrans $ tdtt
--  , Ata.getTrans = S.toMap
--      . S.map (\((p,q),a) -> (((p,q),a),)
--          . Ata.foldrExprOrWith (Ata.foldrExprOrWith (\e -> inf nd e q))
--          . Map.filterWithKey (\(p',a') _ -> p'==p && a'==a && a'/=end)
--          . Tdtt.getTrans $ tdtt)
--      . S.cartesian qs $ as
--}
--  where
--    ndc = Nd.complement nd
--    qs = S.cartesian (getPs tdtt) (Nd.getQs ndc)


--typecheck :: (Eq (s (q1, s (p, q2))), StateSet s,
--  Q p, Q q1, Q q2, Q (s (p, q2)), Alphabet a) =>
--  s a -> Nd.Nd a q1 s -> Nd.Nd a q2 s -> Tdtt a p s -> Bool
--typecheck as inputNd outputNd tdtt = Nd.isEmpty testNd
--  where
--    inferAta = infer as tdtt outputNd
--    inferNd = Ata.toNd as inferAta
--    testNd = inputNd `Nd.intersection` inferNd
