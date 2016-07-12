{-#LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ta.Ata.Types where

import Atom.Types
import Data.Map.Lazy (Map)

data Expr q where
  ExprOr :: Q q => Expr q -> Expr q -> Expr q
  ExprAnd :: Q q => Expr q -> Expr q -> Expr q
  ExprTop :: Expr q
  ExprBottom :: Expr q
  ExprCond :: Q q => Int -> q -> Expr q
deriving instance Q q => Eq (Expr q)
deriving instance Q q => Ord (Expr q)
deriving instance Show q => Show (Expr q)

type Trans a q = Map (q, a) (Expr q)

data Ata a q s = Ata {
    getQs    :: s q
  , getIs    :: s q
  , getFs    :: s q
  , getTrans :: Trans a q
}
