module Ta.Ata.Utils where

import Atom.Types
import Ta.Ata.Types
import Set.Types (StateSet)
import qualified Set.Types as S
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
