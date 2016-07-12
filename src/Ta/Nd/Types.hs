{-#LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ta.Nd.Types where

import Atom.Types
import Data.Map.Lazy (Map)

data Expr q where
  Expr :: Q q => (q, q) -> Expr q
deriving instance (Q q) => Eq (Expr q)
deriving instance (Q q) => Ord (Expr q)
deriving instance (Show q) => Show (Expr q)

type Trans a q s = Map (q, a) (s (Expr q))

data Nd a q s = Nd {
    getQs    :: s q
  , getIs    :: s q
  , getFs    :: s q
  , getTrans :: Trans a q s
}
