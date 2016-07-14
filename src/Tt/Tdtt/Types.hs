{-#LANGUAGE GADTs #-}

module Tt.Tdtt.Types where

import Atom.Types
import Set.Types
import Data.Map (Map)

data Expr where
  ExprA :: Alphabet -> (Expr, Expr) -> Expr
  ExprL :: Expr
  ExprP :: Q -> Int -> Expr
  deriving (Eq, Ord, Show)

type Trans s = Map (Q, Alphabet) (s Expr)

data Tdtt s = Tdtt {
    getPs    :: s Q
  , getP0    :: s Q
  , getTrans :: Trans s
}
