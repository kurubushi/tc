{-#LANGUAGE GADTs #-}

module Atom.Types where

import qualified Data.Set as S

class Ord a => Alphabet a where
  end :: a

class Ord a => Q a
instance (Ord p, Ord q) => Q ((,) p q)
instance Ord q => Q (S.Set q)
  

data Tree a where
  Node :: a -> Tree a -> Tree a -> Tree a
  TEnd :: Tree a
