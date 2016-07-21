{-#LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Atom.Types
import Ta.Ata.Types (Ata(..))
import qualified Ta.Ata.Types as Ata
import qualified Ta.Ata.Utils as Ata
import Ta.Nd.Types (Nd(..))
import qualified Ta.Nd.Types as Nd
import qualified Ta.Nd.Utils as Nd
import Tt.Tdtt.Types (Tdtt(..))
import qualified Tt.Tdtt.Types as Tdtt
import qualified Tt.Tdtt.Utils as Tdtt
import Data.Set (Set)
import qualified Set.Types as SS
import qualified Data.Set as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map


testTypeCheck :: forall p q1 q2 q3 q4 q5.
  (Q p, Q q1, Q q2,
   q3 ~ QD (p,q2),
   q4 ~ QD (Set q3),
   q5 ~ QD (q1,q4)) =>
   Set Alphabet -> Set Alphabet -> Nd q1 Set -> Nd q2 Set -> Tdtt p Set -> Maybe (BTree Alphabet)
testTypeCheck inputAs outputAs inputNd outputNd tdtt =
  if S.null qs
    then Nothing
    else Just $ Nd.unsafeSampleCounterExample testNd memo (head . S.toList $ qs)
  where
  inferAta = Tdtt.infer inputAs outputAs tdtt outputNd :: Ata q3 Set
  inferNd = Ata.toNd inputAs inferAta :: Nd q4 Set
  testNd = inputNd `Nd.intersection` inferNd :: Nd q5 Set
  (memo, qs) = Nd.isEmptyWithQneFollow testNd (Nd.getFs testNd) Map.empty

ae = endAlphabet
a0 = makeAlphabet 0
a1 = makeAlphabet 1
a2 = makeAlphabet 2

q0 = makeQ 0 :: QInt
q1 = makeQ 1 :: QInt
q2 = makeQ 2 :: QInt
q3 = makeQ 3 :: QInt
q4 = makeQ 4 :: QInt
q5 = makeQ 5 :: QInt
qe = makeQ 100 :: QInt

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

-- let inferAta = Tdtt.infer tdtt_01_inA tdtt_01_outA tdtt_01 nd_01_out2 :: Ata.Ata (QD (QInt,QInt)) Set
-- let inferNd = Ata.toNd tdtt_01_inA inferAta :: Nd.Nd (QD (Set (QD (QInt,QInt)))) Set
-- let testNd = nd_01_in `Nd.intersection` inferNd :: Nd.Nd (QD (QInt, QD (Set (QD (QInt,QInt))))) Set
-- let (memo, qs) = Nd.isEmptyWithQneFollow testNd (Nd.getFs testNd) Map.empty
-- Nd.unsafeSampleCounterExample testNd memo (head . S.toList $ qs)



z = makeAlphabet 0
s = makeAlphabet 1

tdtt_02_inA = S.fromList [z, s]
tdtt_02_outA = S.fromList [z, s]

-- 3倍するtdtt
tdtt_02 :: Tdtt QInt Set
tdtt_02 = Tdtt.Tdtt {
    Tdtt.getPs = S.fromList [q0]
  , Tdtt.getP0 = S.fromList [q0]
  , Tdtt.getTrans = tdtt_02_t
}
tdtt_02_t :: Tdtt.Trans QInt Set
tdtt_02_t = Map.fromList
  [((q0,z), S.fromList [Tdtt.ExprA z (Tdtt.ExprL,Tdtt.ExprL)])
  ,((q0,s), S.fromList [Tdtt.ExprA s ((Tdtt.ExprA s ((Tdtt.ExprA s (Tdtt.ExprP q0 1,Tdtt.ExprL)),Tdtt.ExprL)),Tdtt.ExprL)])
  ,((q0,ae), S.fromList [Tdtt.ExprL])] -- わすれないこと！！

-- 2倍
nd_x2 :: Nd.Nd QInt Set
nd_x2 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,qe]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q0,z), S.fromList [Nd.Expr (qe,qe)])
      ,((q0,s), S.fromList [Nd.Expr (q1,qe)])
      ,((q1,s), S.fromList [Nd.Expr (q0,qe)])]
}

-- 5倍
nd_x5 :: Nd.Nd QInt Set
nd_x5 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4,qe]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q0,z), S.fromList [Nd.Expr (qe,qe)])
      ,((q0,s), S.fromList [Nd.Expr (q4,qe)])
      ,((q1,s), S.fromList [Nd.Expr (q0,qe)])
      ,((q2,s), S.fromList [Nd.Expr (q1,qe)])
      ,((q3,s), S.fromList [Nd.Expr (q2,qe)])
      ,((q4,s), S.fromList [Nd.Expr (q3,qe)])]
}
-- 5倍でない
nd_notx5 :: Nd.Nd QInt Set
nd_notx5 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4,qe]
  , Nd.getIs = S.fromList [q1,q2,q3,q4]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q0,z), S.fromList [Nd.Expr (qe,qe)])
      ,((q0,s), S.fromList [Nd.Expr (q4,qe)])
      ,((q1,s), S.fromList [Nd.Expr (q0,qe)])
      ,((q2,s), S.fromList [Nd.Expr (q1,qe)])
      ,((q3,s), S.fromList [Nd.Expr (q2,qe)])
      ,((q4,s), S.fromList [Nd.Expr (q3,qe)])]
}

-- 6倍
nd_x6 :: Nd.Nd QInt Set
nd_x6 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4,q5,qe]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q0,z), S.fromList [Nd.Expr (qe,qe)])
      ,((q0,s), S.fromList [Nd.Expr (q5,qe)])
      ,((q1,s), S.fromList [Nd.Expr (q0,qe)])
      ,((q2,s), S.fromList [Nd.Expr (q1,qe)])
      ,((q3,s), S.fromList [Nd.Expr (q2,qe)])
      ,((q4,s), S.fromList [Nd.Expr (q3,qe)])
      ,((q5,s), S.fromList [Nd.Expr (q4,qe)])]
}
-- 6倍でない
nd_notx6 :: Nd.Nd QInt Set
nd_notx6 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4,q5,qe]
  , Nd.getIs = S.fromList [q1,q2,q3,q4,q5]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q0,z), S.fromList [Nd.Expr (qe,qe)])
      ,((q0,s), S.fromList [Nd.Expr (q5,qe)])
      ,((q1,s), S.fromList [Nd.Expr (q0,qe)])
      ,((q2,s), S.fromList [Nd.Expr (q1,qe)])
      ,((q3,s), S.fromList [Nd.Expr (q2,qe)])
      ,((q4,s), S.fromList [Nd.Expr (q3,qe)])
      ,((q5,s), S.fromList [Nd.Expr (q4,qe)])]
}

-- Tdtt.typecheck tdtt_02_inA tdtt_02_outA nd_02_in nd_x6 tdtt_02
-- Tdtt.typecheck tdtt_02_inA tdtt_02_outA nd_02_in nd_x5 tdtt_02
-- let inferAta = Tdtt.infer tdtt_02_inA tdtt_02_outA tdtt_02 nd_x5 :: Ata.Ata (QD (QInt,QInt)) Set
-- let inferNd = Ata.toNd tdtt_02_inA inferAta :: Nd.Nd (QD (Set (QD (QInt,QInt)))) Set
-- let testNd = nd_02_in `Nd.intersection` inferNd :: Nd.Nd (QD (QInt, QD (Set (QD (QInt,QInt))))) Set
-- let (memo, qs) = Nd.isEmptyWithQneFollow testNd (Nd.getFs testNd) Map.empty
-- Nd.unsafeSampleCounterExample testNd memo (head . S.toList $ qs)




-- fibonacci

fib  = makeQ 0 :: QInt
fib' = makeQ 1 :: QInt
-- fib 0 = 1
-- fib 1 = 2
-- fib 2 = 3
-- fib 3 = 5
-- fib 4 = 8
-- fib 5 = 13
-- fib 6 = 21
-- fib 7 = 34
-- fib 8 = 55

a = makeAlphabet 0
fibAlphabet = S.fromList [a]

-- フィボナッチ数を計算．2つずれている．
-- 木はa(a(a(a(#,#),#),#),#)のようなものを想定する．
-- 入力側ではaの数がその木の自然数を意味し，
-- 出力側では#の数はその木の自然数を意味する．
tdtt_03 :: Tdtt QInt Set
tdtt_03 = Tdtt.Tdtt {
    Tdtt.getPs = S.fromList [fib, fib']
  , Tdtt.getP0 = S.fromList [fib]
  , Tdtt.getTrans = tdtt_03_t
}
tdtt_03_t :: Tdtt.Trans QInt Set
tdtt_03_t = Map.fromList
  [((fib ,a ), S.fromList [Tdtt.ExprA a (Tdtt.ExprP fib 1, Tdtt.ExprP fib' 1)])
  ,((fib ,ae), S.fromList [Tdtt.ExprL])
  ,((fib',a ), S.fromList [Tdtt.ExprP fib 1])
  ,((fib',ae), S.fromList [Tdtt.ExprL])]

-- 入力側，3割って1余るもの
nd_fib_in_3amari1 :: Nd.Nd QInt Set
nd_fib_in_3amari1 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,qe]
  , Nd.getIs = S.fromList [q1]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q1,a), S.fromList [Nd.Expr (qe,qe), Nd.Expr (q0,qe)])
      ,((q2,a), S.fromList [Nd.Expr (q1,qe)])
      ,((q0,a), S.fromList [Nd.Expr (q2,qe)])]
}

-- 出力側，偶数
nd_fib_out_even :: Nd.Nd QInt Set
nd_fib_out_even = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [q1]
  , Nd.getTrans = Map.fromList
      [((q0,a), S.fromList [Nd.Expr (q1,q1), Nd.Expr (q0,q0)])
      ,((q1,a), S.fromList [Nd.Expr (q0,q1), Nd.Expr (q1,q0)])]
}

-- 出力側，奇数
nd_fib_out_odd :: Nd.Nd QInt Set
nd_fib_out_odd = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1]
  , Nd.getIs = S.fromList [q1]
  , Nd.getFs = S.fromList [q1]
  , Nd.getTrans = Map.fromList
      [((q0,a), S.fromList [Nd.Expr (q1,q1), Nd.Expr (q0,q0)])
      ,((q1,a), S.fromList [Nd.Expr (q0,q1), Nd.Expr (q1,q0)])]
}

-- 入力側，5で割って3余る
nd_fib_in_5amari3 :: Nd.Nd QInt Set
nd_fib_in_5amari3 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4,qe]
  , Nd.getIs = S.fromList [q3]
  , Nd.getFs = S.fromList [qe]
  , Nd.getTrans = Map.fromList
      [((q1,a), S.fromList [Nd.Expr (qe,qe), Nd.Expr (q0,qe)])
      ,((q2,a), S.fromList [Nd.Expr (q1,qe)])
      ,((q3,a), S.fromList [Nd.Expr (q2,qe)])
      ,((q4,a), S.fromList [Nd.Expr (q3,qe)])
      ,((q0,a), S.fromList [Nd.Expr (q4,qe)])]
}

-- 出力側，5の倍数
nd_fib_out_x5 :: Nd.Nd QInt Set
nd_fib_out_x5 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4]
  , Nd.getIs = S.fromList [q0]
  , Nd.getFs = S.fromList [q1]
  , Nd.getTrans = Map.fromList
      [((q1,a), S.fromList . map Nd.Expr $ [(q0,q1),(q1,q0),(q2,q4),(q3,q3),(q4,q2)])
      ,((q2,a), S.fromList . map Nd.Expr $ [(q0,q2),(q1,q1),(q2,q0),(q3,q4),(q4,q3)])
      ,((q3,a), S.fromList . map Nd.Expr $ [(q0,q3),(q1,q2),(q2,q1),(q3,q0),(q4,q4)])
      ,((q4,a), S.fromList . map Nd.Expr $ [(q0,q4),(q1,q3),(q2,q2),(q3,q1),(q4,q0)])
      ,((q0,a), S.fromList . map Nd.Expr $ [(q0,q0),(q1,q4),(q2,q3),(q3,q2),(q4,q1)])]
}

-- 出力側，5で割って1余る
nd_fib_out_x5_1 :: Nd.Nd QInt Set
nd_fib_out_x5_1 = Nd.Nd {
    Nd.getQs = S.fromList [q0,q1,q2,q3,q4]
  , Nd.getIs = S.fromList [q1]
  , Nd.getFs = S.fromList [q1]
  , Nd.getTrans = Map.fromList
      [((q1,a), S.fromList . map Nd.Expr $ [(q0,q1),(q1,q0),(q2,q4),(q3,q3),(q4,q2)])
      ,((q2,a), S.fromList . map Nd.Expr $ [(q0,q2),(q1,q1),(q2,q0),(q3,q4),(q4,q3)])
      ,((q3,a), S.fromList . map Nd.Expr $ [(q0,q3),(q1,q2),(q2,q1),(q3,q0),(q4,q4)])
      ,((q4,a), S.fromList . map Nd.Expr $ [(q0,q4),(q1,q3),(q2,q2),(q3,q1),(q4,q0)])
      ,((q0,a), S.fromList . map Nd.Expr $ [(q0,q0),(q1,q4),(q2,q3),(q3,q2),(q4,q1)])]
}
