{-#LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tt.Tdtt.Types where

import Atom.Types
import Set.Types
import Data.Map (Map)

data Expr a p where
  ExprA :: Alphabet a => a -> (Expr a p, Expr a p) -> Expr a p
  ExprL :: Expr a p
  ExprP :: Q p => p -> Int -> Expr a p
deriving instance (Alphabet a, Q p) => Eq (Expr a p)
deriving instance (Alphabet a, Q p) => Ord (Expr a p)
deriving instance (Show a, Show p) => Show (Expr a p)

type Trans a p s = Map (p, a) (s (Expr a p))

data Tdtt a p s = Tdtt {
    getPs    :: s p
  , getP0    :: s p
  , getTrans :: Trans a p s
}
