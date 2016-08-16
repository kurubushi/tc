{-#LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts#-}

import System.Environment
import System.IO
import Atom.Types
import Control.Monad
import System.IO.Unsafe
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
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

inA = S.fromList [a0,a1]
outA = S.fromList [a0,a1,a2]
q0 = QD "q0"
q1 = QD "q1"
a0 = makeAlphabet "a0"
a1 = makeAlphabet "a1"
a2 = makeAlphabet "a2"
inc = QD "inc"

allnd = Nd {
    Nd.getQs = S.fromList [q0]
 ,  Nd.getIs = S.fromList [q0]
 ,  Nd.getFs = S.fromList [q0]
 ,  Nd.getTrans = trans
}
  where
  trans (q,q') a = S.singleton q0

nota2 = Nd {
    Nd.getQs = S.fromList [q0,q1]
 ,  Nd.getIs = S.fromList [q0]
 ,  Nd.getFs = S.fromList [q0]
 ,  Nd.getTrans = trans
}
  where
  trans (q,q') a
    | a==a0 = S.singleton q0
    | a==a1 = S.singleton q0
    | a==a2 = S.singleton q1
    | otherwise = S.empty

nota2' = Nd {
    Nd.getQs = S.fromList [q0]
 ,  Nd.getIs = S.fromList [q0]
 ,  Nd.getFs = S.fromList [q0]
 ,  Nd.getTrans = trans
}
  where
  trans (q,q') a
    | a==a0 = S.singleton q0
    | a==a1 = S.singleton q0
    | otherwise = S.empty

maybeInc = Tdtt {
    Tdtt.getPs = S.fromList [inc]
  , Tdtt.getP0 = S.fromList [inc]
  , Tdtt.getTrans = trans
}
  where
  trans q a
    | a==a0 = S.singleton $ Tdtt.ExprA a1 (Tdtt.ExprP inc 1, Tdtt.ExprP inc 2)
    | a==a1 = S.singleton $ Tdtt.ExprA a2 (Tdtt.ExprP inc 1, Tdtt.ExprP inc 2)
    | isEnd a = S.singleton Tdtt.ExprL
    | otherwise = S.empty



main :: IO ()
main =
  print $ testTypeCheck inA outA allnd nota2' maybeInc
  -- (4,Just (Free (BTNode' a1 (Free BTEnd') (Free BTEnd'))))
  --   要素はなんでもいい．sndがJustであることがテストの成功を意味する．

testTypeCheck :: forall p q1 q2 q3 q4 q5.
  (Q p, Q q1, Q q2,
   q3 ~ QD (p,q2),
   q4 ~ QD (Set q3),
   q5 ~ QD (q1,q4)) =>
   Set Alphabet -> Set Alphabet -> Nd q1 Set -> Nd q2 Set -> Tdtt p Set -> (Int, Maybe (BTree Alphabet))
testTypeCheck inputAs outputAs inputNd outputNd tdtt
  | S.null qs = (testNdSize, Nothing)
  | otherwise = (testNdSize, Nd.sampleCounterExample testNd memo (head . S.toList $ qs))
  where
  inferAta = Tdtt.infer inputAs outputAs tdtt outputNd :: Ata q3 Set
  inferNd = Ata.toNd inputAs inferAta :: Nd q4 Set
  testNd = inputNd `Nd.intersection` inferNd :: Nd q5 Set
  testNdSize = S.size $ Nd.getQs testNd
  (memo, qs) = Nd.isEmptyWithQneFollow outputAs testNd (Nd.getFs testNd) Map.empty
