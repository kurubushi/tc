import Atom.Types
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import qualified Tt.Tdtt.Types as Tdtt
import qualified Tt.Tdtt.Utils as Tdtt
import Data.Set (Set)
import qualified Set.Types as SS
import qualified Data.Set as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

--newtype Q = Q Int deriving (Ord, Eq, Show)
--instance Atom.Q Q
--
--newtype A = A Char deriving (Ord, Eq, Show)
--instance Atom.Alphabet A where
--  end = A '#'
--
--at911 :: Ata.Trans A Q
--at911 = Map.fromList
--  [((Q 0, A 'a'), (Ata.ExprCond 1 (Q 1) `Ata.ExprAnd` Ata.ExprCond 2 (Q 2)) `Ata.ExprOr` ((Ata.ExprCond 1 (Q 2)) `Ata.ExprAnd` Ata.ExprCond 2 (Q 1)))
--  ,((Q 1, A 'b'), (Ata.ExprCond 1 (Q 3) `Ata.ExprAnd` Ata.ExprCond 2 (Q 3)))
--  ,((Q 2, A 'c'), (Ata.ExprCond 1 (Q 3) `Ata.ExprAnd` Ata.ExprCond 2 (Q 3)))
--  -- ここから全域化
--  ,((Q 0, A 'b'), Ata.ExprBottom)
--  ,((Q 0, A 'c'), Ata.ExprBottom)
--  ,((Q 1, A 'a'), Ata.ExprBottom)
--  ,((Q 1, A 'c'), Ata.ExprBottom)
--  ,((Q 2, A 'a'), Ata.ExprBottom)
--  ,((Q 2, A 'b'), Ata.ExprBottom)]
--
--a911 :: Ata A Q Set
--a911 = Ata {
--    Ata.getQs = S.fromList . map Q $ [0, 1, 2, 3]
--  , Ata.getIs = S.fromList [Q 0]
--  , Ata.getFs = S.fromList [Q 3]
--  , Ata.getTrans = at911
--}
--
--t431_1 :: Atom.Tree A
--t431_1 = Atom.Node (A 'a')
--  (Atom.Node (A 'b')
--    Atom.TEnd
--    Atom.TEnd)
--  (Atom.Node (A 'c')
--    Atom.TEnd
--    Atom.TEnd)
--t431_2 :: Atom.Tree A
--t431_2 = Atom.Node (A 'a')
--  (Atom.Node (A 'c')
--    Atom.TEnd
--    Atom.TEnd)
--  (Atom.Node (A 'b')
--    Atom.TEnd
--    Atom.TEnd)
--t431_3 :: Atom.Tree A
--t431_3 = Atom.Node (A 'a')
--  (Atom.Node (A 'b')
--    Atom.TEnd
--    Atom.TEnd)
--  (Atom.Node (A 'b')
--    Atom.TEnd
--    Atom.TEnd)
--t431_4 = (Atom.Node (A 'b')
--  Atom.TEnd
--  Atom.TEnd)

alphabet = S.fromList [makeAlphabet 0, makeAlphabet 1]

tdttt_1132 = Map.fromList
  [((makeQ 0, makeAlphabet 0), S.fromList [Tdtt.ExprA (makeAlphabet 0) ((Tdtt.ExprP (makeQ 0) 1),(Tdtt.ExprP (makeQ 0) 2))])
  ,((makeQ 0, makeAlphabet 1), S.fromList [Tdtt.ExprA (makeAlphabet 0) ((Tdtt.ExprP (makeQ 0) 1),(Tdtt.ExprP (makeQ 0) 2))
                             ,Tdtt.ExprA (makeAlphabet 1) ((Tdtt.ExprP (makeQ 0) 1),(Tdtt.ExprP (makeQ 0) 2))])
  ,((makeQ 0, AlphabetEnd), S.fromList [Tdtt.ExprL])]

tdtt_1132 = Tdtt.Tdtt {
    Tdtt.getPs = S.fromList [makeQ 0]
  , Tdtt.getP0 = S.fromList [makeQ 0]
  , Tdtt.getTrans = tdttt_1132
}

nd_1132' = Nd.Nd {
    Nd.getQs = S.fromList [makeQ 0, makeQ 1]
  , Nd.getIs = S.fromList [makeQ 1]
  , Nd.getFs = S.fromList [makeQ 1]
  , Nd.getTrans = Map.fromList
      [((makeQ 0, makeAlphabet 0), S.fromList [(Nd.Expr (makeQ 0, makeQ 1)), (Nd.Expr (makeQ 1, makeQ 0))])
      ,((makeQ 0, makeAlphabet 1), S.fromList [Nd.Expr (makeQ 1, makeQ 1)])
      ,((makeQ 1, makeAlphabet 0), S.fromList [Nd.Expr (makeQ 1, makeQ 1)])
      ,((makeQ 1, makeAlphabet 1), S.empty)]
}

nd_1132 = Nd.Nd {
    Nd.getQs = S.fromList [makeQ 0]
  , Nd.getIs = S.fromList [makeQ 0]
  , Nd.getFs = S.fromList [makeQ 0]
  , Nd.getTrans = Map.fromList
      [((makeQ 0, makeAlphabet 0), S.fromList [(Nd.Expr (makeQ 0, makeQ 0))])]
}
