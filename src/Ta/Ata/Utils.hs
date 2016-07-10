{-#LANGUAGE TupleSections #-}

module Ta.Ata.Utils where

import Atom.Types
import Ta.Ata.Types
import Set.Types (StateSet)
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import qualified Set.Types as S
import qualified Set.Utils as S
import qualified Data.Map as Map

dnf :: (Q q, StateSet s, Ord (s q)) => Expr q -> s (s q, s q)
dnf ExprTop = S.fromList [(S.empty, S.empty)]
dnf ExprBottom = S.empty
dnf (ExprAnd e1 e2) = S.fromList [(s1 `S.union` s1', s2 `S.union` s2')
    | (s1,s2) <- S.toList (dnf e1), (s1',s2') <- S.toList (dnf e2)]
dnf (ExprOr e1 e2) = dnf e1 `S.union` dnf e2
dnf (ExprCond 1 q) = S.fromList [(S.fromList [q], S.empty)]
dnf (ExprCond 2 q) = S.fromList [(S.empty, S.fromList [q])]

accept :: (Alphabet a, Q q, StateSet s, Ord (s q)) =>
  Ata a q s -> Tree a -> Bool
accept ata t = S.notNull
  . S.filter (\q -> acceptIn ata t (S.fromList [q]))
  . getIs $ ata

acceptIn :: (Alphabet a, Q q, StateSet s, Ord (s q)) =>
  Ata a q s -> Tree a -> s q -> Bool
acceptIn ata TEnd s = s `S.isSubsetOf` getFs ata
acceptIn ata (Node a t1 t2) s = S.notNull
  . S.filter (\(s1,s2) -> S.notNull s1 && S.notNull s2
      && acceptIn ata t1 s1 && acceptIn ata t2 s2)
  . dnf 
  . Map.foldr ExprAnd ExprTop
  . Map.filterWithKey (\(q,a') _ -> a == a' && q `S.member` s)
  . getTrans $ ata

isTop :: Q q => Expr q -> Bool
isTop (ExprOr e1 e2) = isTop e1 || isTop e2
isTop (ExprAnd e1 e2) = isTop e1 && isTop e2
isTop ExprTop = True
isTop ExprBottom = False
isTop (ExprCond _ _) = False

isNotTop :: Q q => Expr q -> Bool
isNotTop = not . isTop

foldrExprOrWith :: (Foldable f, Q q) => (a -> Expr q) -> f a -> Expr q
foldrExprOrWith f = foldr (\x acc -> f x `ExprOr` acc) ExprBottom

foldrExprAndWith :: (Foldable f, Q q) => (a -> Expr q) -> f a -> Expr q
foldrExprAndWith f = foldr (\x acc -> f x `ExprAnd` acc) ExprTop


toNd :: (Alphabet a, Q q, StateSet s, Q (s q)) => s a -> Ata a q s -> Nd.Nd a (s q) s
toNd as ata = Nd.Nd {
    Nd.getQs = qs
  , Nd.getIs = S.map (\q -> S.fromList [q]) (getIs ata)
  , Nd.getFs = S.filter (\s -> s `S.isSubsetOf` getFs ata) qs
--  , Nd.getFs = S.filter (\s -> S.notNull s && s `S.isSubsetOf` getFs ata) qs
  , Nd.getTrans = S.toMap . map makeSubs -- Ord is too restrict
      . S.toList
      . S.cartesian qs $ as
}
  where
    qs = S.powerset . getQs $ ata
    makeSubs (s,a) = ((s,a),) . S.map Nd.Expr . dnf 
      . foldrExprAndWith (\q -> Map.findWithDefault ExprBottom (q,a) (getTrans ata))
      . S.filter (`S.member` s)
      . getQs $ ata
