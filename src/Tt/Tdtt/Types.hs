{-#LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tt.Tdtt.Types where

import Atom.Types
import Set.Types
import Data.Map (Map)

data TdttExpr a p where
  TdttExprA :: Alphabet a => a -> (TdttExpr a p, TdttExpr a p) -> TdttExpr a p
  TdttExprL :: TdttExpr a p
  TdttExprP :: Q p => p -> Int -> TdttExpr a p
deriving instance (Alphabet a, Q p) => Eq (TdttExpr a p)
deriving instance (Alphabet a, Q p) => Ord (TdttExpr a p)
deriving instance (Show a, Show p) => Show (TdttExpr a p)

type TdttTrans a p s = Map (p, a) (s (TdttExpr a p))

data Tdtt a p s = Tdtt {
    getPs    :: s p
  , getP0    :: s p
  , getTrans :: TdttTrans a p s
}
