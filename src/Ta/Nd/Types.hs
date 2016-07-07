{-#LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ta.Nd.Types where

import Atom.Types
import Data.Map.Lazy (Map)

data Expr a q where
  Expr :: (Alphabet a, Q q) => a -> (q, q) -> Expr a q
deriving instance (Alphabet a, Q q) => Eq (Expr a q)
deriving instance (Alphabet a, Q q) => Ord (Expr a q)
deriving instance (Show a, Show q) => Show (Expr a q)

type Trans a q s = Map (q, a) (s (Expr a q))

data Nd a q s = Nd {
    getQs    :: s q
  , getIs    :: s q
  , getFs    :: s q
  , getTrans :: Trans a q s
}
