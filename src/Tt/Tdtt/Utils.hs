{-#LANGUAGE FlexibleContexts #-}

module Tt.Tdtt.Utils where

import Atom.Types
import Set.Types (StateSet)
import qualified Set.Types as S
import Tt.Tdtt.Types
import qualified Data.Map as Map
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import Ta.Nd.Types (Nd(..))
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import Data.Maybe (fromMaybe)

inf :: (Alphabet a, Q q, Q p, StateSet s) =>
  Nd a q s -> Expr a p -> q -> Ata.Expr (p,q)
inf nd ExprL q
  | q `S.member` Nd.getFs nd = Ata.ExprTop
  | otherwise = Ata.ExprBottom
inf nd (ExprA a (e1,e2)) q =
  maybe Ata.ExprBottom (S.foldr aux Ata.ExprBottom) 
  . Map.lookup (q, a)
  . Nd.getTrans $ nd
  where
    aux (Nd.Expr (q1,q2)) acc = 
      (inf nd e1 q1 `Ata.ExprAnd` inf nd e2 q2) `Ata.ExprOr` acc
inf nd (ExprP p n) q = Ata.ExprCond n (p,q)
