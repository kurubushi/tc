{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}

module Ta.Nd.Types where

import Atom.Types
import Data.Map.Lazy (Map)

data Expr q where
  Expr :: Q q => (q, q) -> Expr q
deriving instance Eq q => Eq (Expr q)
deriving instance Ord q => Ord (Expr q)
deriving instance Show q => Show (Expr q)

type Trans q s = Map (q, Alphabet) (s (Expr q))

data Nd q s = Nd {
    getQs    :: s q
  , getIs    :: s q
  , getFs    :: s q
  , getTrans :: Trans q s
}
