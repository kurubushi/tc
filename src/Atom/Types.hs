{-#LANGUAGE GADTs #-}

module Atom.Types where

class Ord a => Alphabet a where
  end :: a

class Ord a => Q a

data Tree a where
  Node :: a -> Tree a -> Tree a -> Tree a
  TEnd :: Tree a
