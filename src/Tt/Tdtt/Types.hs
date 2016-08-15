{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}

module Tt.Tdtt.Types where

import Atom.Types
import Set.Types
import Data.Map.Lazy (Map)

data Expr p where
  ExprA :: Alphabet -> (Expr p, Expr p) -> Expr p
  ExprL :: Expr p
  ExprP :: Q p => p -> Int -> Expr p
deriving instance Eq p => Eq (Expr p)
deriving instance Ord p => Ord (Expr p)
deriving instance Show p => Show (Expr p)

type Trans p s = p -> Alphabet -> (s (Expr p))

data Tdtt p s = Tdtt {
    getPs    :: s p
  , getP0    :: s p
  , getTrans :: Trans p s
}
