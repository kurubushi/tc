module Set.Types where

import qualified Prelude
import Prelude hiding (
  null, filter, map)
import qualified Data.Set as S
import qualified Data.List as L

class Foldable s => StateSet s where
  empty :: s a
  null :: s a -> Bool
  null = not . notNull
  notNull :: s a -> Bool
  notNull = not . null
  size :: s a -> Int
  union :: Ord a => s a -> s a -> s a
  member :: Ord a => a -> s a -> Bool
  isSubsetOf :: Ord a => s a -> s a -> Bool
  intersection :: Ord a => s a -> s a -> s a
  difference :: Ord a => s a -> s a -> s a
  cartesian :: s a -> s b -> s (a,b)
  map :: Ord b => (a -> b) -> s a -> s b
  filter :: (a -> Bool) -> s a -> s a
  powerset :: Ord a => s a -> s (s a)
  fromList :: Ord a => [a] -> s a
  toList :: s a -> [a]

instance StateSet S.Set where
  empty = S.empty
  null = S.null
  size = S.size
  union = S.union
  member = S.member
  isSubsetOf = S.isSubsetOf
  intersection = S.intersection
  difference = S.difference
  cartesian xs ys = S.fromDistinctAscList -- is safe
    [(x, y) | x <- S.toAscList xs, y <- S.toAscList ys]
  map = S.map
  filter = S.filter
  powerset = S.fromList . map S.fromList . powerset . S.toAscList
  fromList = S.fromList
  toList = S.toList

instance StateSet [] where
  empty = []
  null = Prelude.null
  size = length
  union = (++)
  member = elem
  isSubsetOf xs ys = xs `member` powerset ys
  intersection xs ys = [x | x <- xs, x `elem` ys]
  difference xs ys = [x | x <- xs, x `notElem` ys]
  cartesian xs ys = [(x, y) | x <- xs, y <- ys]
  map = Prelude.map
  filter = Prelude.filter
  powerset = L.subsequences
  fromList = id
  toList = id
