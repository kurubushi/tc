module Set.Types where

import qualified Prelude
import Prelude hiding (
  null, filter, foldr)
import qualified Data.Set as S
import qualified Data.List as L

class Foldable s => StateSet s where
  empty :: s a
  null :: Ord a => s a -> Bool
  null = not . notNull
  notNull :: Ord a => s a -> Bool
  notNull = not . null
  union :: Ord a => s a -> s a -> s a
  member :: Ord a => a -> s a -> Bool
  isSubsetOf :: Ord a => s a -> s a -> Bool
  intersection :: Ord a => s a -> s a -> s a
  difference :: Ord a => s a -> s a -> s a
  cartesian :: (Ord a, Ord b) => s a -> s b -> s (a,b)
  filter :: (Ord a) => (a -> Bool) -> s a -> s a
  foldr :: (Ord a) => (a -> b -> b) -> b -> s a -> b
  powerset :: Ord a => s a -> s (s a)
  fromList :: Ord a => [a] -> s a
  toList :: Ord a => s a -> [a]

instance StateSet S.Set where
  empty = S.empty
  null = S.null
  union = S.union
  member = S.member
  isSubsetOf = S.isSubsetOf
  intersection = S.intersection
  difference = S.difference
  cartesian xs ys = S.fromDistinctAscList
    [(x, y) | x <- S.toAscList xs, y <- S.toAscList ys]
  filter = S.filter
  foldr = S.foldr
  powerset = S.fromList . map S.fromList . powerset . S.toList
  fromList = S.fromList
  toList = S.toList

instance StateSet [] where
  empty = []
  null = Prelude.null
  union = (++)
  member = elem
  isSubsetOf xs ys = xs `member` powerset ys
  intersection xs ys = [x | x <- xs, x `elem` ys]
  difference xs ys = [x | x <- xs, x `notElem` ys]
  cartesian xs ys = [(x, y) | x <- xs, y <- ys]
  filter = Prelude.filter
  foldr = Prelude.foldr
  powerset = L.subsequences
  fromList = id
  toList = id
