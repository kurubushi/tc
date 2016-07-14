{-#LANGUAGE GADTs #-}

module Ta.Ata.Types where

import Atom.Types
import Data.Map.Lazy (Map)

data Expr where
  ExprOr :: Expr -> Expr -> Expr
  ExprAnd :: Expr -> Expr -> Expr
  ExprTop :: Expr
  ExprBottom :: Expr
  ExprCond :: Int -> Q -> Expr
  deriving (Ord, Eq, Show)

type Trans = Map (Q, Alphabet) Expr

data Ata s = Ata {
    getQs    :: s Q
  , getIs    :: s Q
  , getFs    :: s Q
  , getTrans :: Trans
}
