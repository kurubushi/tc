{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeFamilies #-}

module Ta.Ata.Utils where

import Atom.Types
import Ta.Ata.Types
import Set.Types (StateSet)
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import qualified Set.Types as S
import qualified Set.Utils as S
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

dnf :: (StateSet s, Q q, Ord (s q)) => Expr q -> s (s q, s q)
dnf ExprTop = S.fromList [(S.empty, S.empty)]
dnf ExprBottom = S.empty
dnf (ExprAnd e1 e2) = S.fromList [(s1 `S.union` s1', s2 `S.union` s2')
    | (s1,s2) <- S.toList (dnf e1), (s1',s2') <- S.toList (dnf e2)]
dnf (ExprOr e1 e2) = dnf e1 `S.union` dnf e2
dnf (ExprCond 1 q) = S.fromList [(S.fromList [q], S.empty)]
dnf (ExprCond 2 q) = S.fromList [(S.empty, S.fromList [q])]

dash :: (StateSet s, Q q) => (s q, s q) -> Expr q -> Bool
(s1,s2) `dash` ExprOr e1 e2 = (s1,s2) `dash` e1 || (s1,s2) `dash` e2
(s1,s2) `dash` ExprAnd e1 e2 = (s1,s2) `dash` e1 && (s1,s2) `dash` e2
(s1,s2) `dash` ExprTop = True
(s1,s2) `dash` ExprBottom = False
(s1,s2) `dash` ExprCond 1 q = q `S.member` s1
(s1,s2) `dash` ExprCond 2 q = q `S.member` s2

--accept :: (StateSet s, Q q, Ord (s q)) =>
--  Ata q s -> BTree Alphabet -> Bool
--accept ata t = S.notNull
--  . S.filter (\q -> acceptIn ata t (S.fromList [q]))
--  . getIs $ ata
--
--acceptIn :: (StateSet s, Q q, Ord (s q)) =>
--  Ata q s -> BTree Alphabet -> s q -> Bool
--acceptIn ata t s
--  | isBTEnd t = s `S.isSubsetOf` getFs ata
--  | otherwise = let (a,t1,t2) = unsafeGetElems t in -- t is not End
--      S.notNull
--      . S.filter (\(s1,s2) -> S.notNull s1 && S.notNull s2
--          && acceptIn ata t1 s1 && acceptIn ata t2 s2)
--      . dnf 
--      . Map.foldr ExprAnd ExprTop
--      . Map.filterWithKey (\(q,a') _ -> a == a' && q `S.member` s)
--      . getTrans $ ata
--
isTop :: Expr q -> Bool
isTop (ExprOr e1 e2) = isTop e1 || isTop e2
isTop (ExprAnd e1 e2) = isTop e1 && isTop e2
isTop ExprTop = True
isTop ExprBottom = False
isTop (ExprCond _ _) = False

isNotTop :: Expr q -> Bool
isNotTop = not . isTop

foldrExprOrWith :: (Foldable f, Q q) => (a -> Expr q) -> f a -> Expr q
foldrExprOrWith f = foldr (\x acc -> f x `ExprOr` acc) ExprBottom

foldrExprAndWith :: (Foldable f, Q q) => (a -> Expr q) -> f a -> Expr q
foldrExprAndWith f = foldr (\x acc -> f x `ExprAnd` acc) ExprTop


toNd :: (StateSet s, Q q, Ord (s q), Q q', QElem q' ~ s q) =>
  s Alphabet -> Ata q s -> Nd.Nd q' s
toNd as ata = Nd.Nd {
    Nd.getQs = S.map conv qs
  , Nd.getIs = S.map conv is
  , Nd.getFs = S.map conv fs
  , Nd.getTrans = \(q1,q2) a -> single . conv . fromMaybe S.empty
      $ (\s1 s2 -> S.filter (\q -> (s1,s2) `dash` (getTrans ata) q a) $ getQs ata)
        <$> getQ q1 <*> getQ q2
}
  where
    conv = makeQ
    qs = S.powerset . getQs $ ata
    is = S.filter (\s -> S.notNull (s `S.intersection` getIs ata)) qs
    fs = S.fromList [getFs ata]
    single x = S.fromList [x]
