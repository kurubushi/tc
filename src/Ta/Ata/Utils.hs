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

dnf :: (StateSet s, Ord (s Q)) => Expr -> s (s Q, s Q)
dnf ExprTop = S.fromList [(S.empty, S.empty)]
dnf ExprBottom = S.empty
dnf (ExprAnd e1 e2) = S.fromList [(s1 `S.union` s1', s2 `S.union` s2')
    | (s1,s2) <- S.toList (dnf e1), (s1',s2') <- S.toList (dnf e2)]
dnf (ExprOr e1 e2) = dnf e1 `S.union` dnf e2
dnf (ExprCond 1 q) = S.fromList [(S.fromList [q], S.empty)]
dnf (ExprCond 2 q) = S.fromList [(S.empty, S.fromList [q])]

accept :: (StateSet s, Ord (s Q)) =>
  Ata s -> BTree Alphabet -> Bool
accept ata t = S.notNull
  . S.filter (\q -> acceptIn ata t (S.fromList [q]))
  . getIs $ ata

acceptIn :: (StateSet s, Ord (s Q)) =>
  Ata s -> BTree Alphabet -> s Q -> Bool
acceptIn ata BTEnd s = s `S.isSubsetOf` getFs ata
acceptIn ata (BTNode a t1 t2) s = S.notNull
  . S.filter (\(s1,s2) -> S.notNull s1 && S.notNull s2
      && acceptIn ata t1 s1 && acceptIn ata t2 s2)
  . dnf 
  . Map.foldr ExprAnd ExprTop
  . Map.filterWithKey (\(q,a') _ -> a == a' && q `S.member` s)
  . getTrans $ ata

isTop :: Expr -> Bool
isTop (ExprOr e1 e2) = isTop e1 || isTop e2
isTop (ExprAnd e1 e2) = isTop e1 && isTop e2
isTop ExprTop = True
isTop ExprBottom = False
isTop (ExprCond _ _) = False

isNotTop :: Expr -> Bool
isNotTop = not . isTop

foldrExprOrWith :: Foldable f => (a -> Expr) -> f a -> Expr
foldrExprOrWith f = foldr (\x acc -> f x `ExprOr` acc) ExprBottom

foldrExprAndWith :: Foldable f => (a -> Expr) -> f a -> Expr
foldrExprAndWith f = foldr (\x acc -> f x `ExprAnd` acc) ExprTop


toNd :: (StateSet s, Ord (s Q)) => s Alphabet -> Ata s -> Nd.Nd s
toNd as ata = Nd.Nd {
    Nd.getQs = S.map conv qs
  , Nd.getIs = S.map conv is
  , Nd.getFs = S.map conv fs
--  , Nd.getFs = S.filter (\s -> S.notNull s && s `S.isSubsetOf` getFs ata) qs
  , Nd.getTrans = S.toMap . map makeSubs -- Ord is too restrict
      . S.toList
      . S.cartesian qs $ as
}
  where
    conv = unsafeConverMap qs
    convPair (x,y) = (conv x, conv y)
    qs = S.filter S.notNull . S.powerset . getQs $ ata
    is = S.map (\q -> S.fromList [q]) (getIs ata)
    fs = S.filter (\s -> s `S.isSubsetOf` getFs ata) qs
    makeSubs (s,a) = ((conv s,a),) . S.map (Nd.Expr . convPair) . dnf 
      . foldrExprAndWith (\q -> Map.findWithDefault ExprBottom (q,a) (getTrans ata))
      . S.filter (`S.member` s)
      . getQs $ ata
