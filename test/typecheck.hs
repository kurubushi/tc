import Atom.Types
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import Tt.Tdtt.Types (Tdtt)
import qualified Tt.Tdtt.Types as Tdtt
import qualified Tt.Tdtt.Utils as Tdtt
import Data.Set (Set)
import qualified Set.Types as SS
import qualified Data.Set as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

ae = endAlphabet
a0 = makeAlphabet 0
a1 = makeAlphabet 1
a2 = makeAlphabet 2

q0 = makeQ 0 :: QInt
q1 = makeQ 1 :: QInt
q2 = makeQ 2 :: QInt

tdtt_01_inA = S.fromList [a0, a1]
tdtt_01_outA = S.fromList [a0, a1, a2]

-- a0->a1, a1->a2にするtdtt
tdtt_01 :: Tdtt QInt Set
tdtt_01 = Tdtt.Tdtt {
    Tdtt.getPs = S.fromList [q0]
  , Tdtt.getP0 = S.fromList [q0]
  , Tdtt.getTrans = tdtt_01_t
}
tdtt_01_t :: Tdtt.Trans QInt Set
tdtt_01_t = Map.fromList
  [((q0,a0), S.fromList [Tdtt.ExprA a1 ((Tdtt.ExprP q0 1),(Tdtt.ExprP q0 2))])
  ,((q0,a1), S.fromList [Tdtt.ExprA a2 ((Tdtt.ExprP q0 1),(Tdtt.ExprP q0 2))])
  ,((q0,ae), S.fromList [Tdtt.ExprL])] -- わすれないこと！！

-- なんでも受理
nd_01_in :: Nd.Nd QInt Set
nd_01_in = Nd.Nd {
    Nd.getQs = S.fromList [q0]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [q0]
  , Nd.getTrans = Map.fromList
      [((q0,a0), S.fromList [Nd.Expr (q0,q0)])
      ,((q0,a1), S.fromList [Nd.Expr (q0,q0)])]
}

-- a0が入っていないときだけ受理する
nd_01_out :: Nd.Nd QInt Set
nd_01_out = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [q0]
  , Nd.getTrans = Map.fromList
      [((q0,a0), S.empty)
      ,((q1,a0), S.fromList [(Nd.Expr (q0,q0)), (Nd.Expr (q0,q1)), (Nd.Expr (q1,q0)), (Nd.Expr (q1,q1))])
      ,((q0,a1), S.fromList [(Nd.Expr (q0,q0))])
      ,((q1,a1), S.fromList [(Nd.Expr (q0,q1)), (Nd.Expr (q1,q0)), (Nd.Expr (q1,q1))])
      ,((q0,a2), S.fromList [(Nd.Expr (q0,q0))])
      ,((q1,a2), S.fromList [(Nd.Expr (q0,q1)), (Nd.Expr (q1,q0)), (Nd.Expr (q1,q1))])]
}

-- a2が入っていないときだけ受理する
nd_01_out2 :: Nd.Nd QInt Set
nd_01_out2 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [q0]
  , Nd.getTrans = Map.fromList
      [((q1,a2), S.fromList [(Nd.Expr (q0,q0)), (Nd.Expr (q0,q1)), (Nd.Expr (q1,q0)), (Nd.Expr (q1,q1))])
      ,((q0,a1), S.fromList [(Nd.Expr (q0,q0))])
      ,((q1,a1), S.fromList [(Nd.Expr (q0,q1)), (Nd.Expr (q1,q0)), (Nd.Expr (q1,q1))])
      ,((q0,a0), S.fromList [(Nd.Expr (q0,q0))])
      ,((q1,a0), S.fromList [(Nd.Expr (q0,q1)), (Nd.Expr (q1,q0)), (Nd.Expr (q1,q1))])]
}
