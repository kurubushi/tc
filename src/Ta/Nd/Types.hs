{-#LANGUAGE GADTs #-}

module Ta.Nd.Types where

import Atom.Types
import Data.Map.Lazy (Map)

data Expr where
  Expr :: (Q, Q) -> Expr
  deriving (Eq, Ord, Show)

type Trans s = Map (Q, Alphabet) (s Expr)

data Nd s = Nd {
    getQs    :: s Q
  , getIs    :: s Q
  , getFs    :: s Q
  , getTrans :: Trans s
}
