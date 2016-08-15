{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE FlexibleContexts #-}

module Ta.Ata.Types where

import Atom.Types
import Set.Types (StateSet)
import Data.Map.Lazy (Map)

data Expr q where
  ExprOr :: Expr q -> Expr q -> Expr q
  ExprAnd :: Expr q -> Expr q -> Expr q
  ExprTop :: Expr q
  ExprBottom :: Expr q
  ExprCond :: Q q => Int -> q -> Expr q
deriving instance Eq q => Eq (Expr q)
deriving instance Ord q => Ord (Expr q)
deriving instance Show q => Show (Expr q)

type Trans q = q -> Alphabet -> Expr q

data Ata q s = Ata {
    getQs    :: s q
  , getIs    :: s q
  , getFs    :: s q
  , getTrans :: Trans q
}
