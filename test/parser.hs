{-#LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.IO
import Atom.Types
import Control.Monad
import System.IO.Unsafe
import qualified Parser.Parser as P
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

main :: IO ()
main = do
  [filename] <- getArgs
  c <- readFile filename
  let Right (m, execs) = P.parseProgram c
  mapM_ (\e -> exec m e >> putStrLn "") execs
  putStrLn "end."

exec :: P.EMap -> P.Exec -> IO ()
exec m ch@(P.Typecheck tdttv indv iav ondv oav) = do
  putStr "Start!: "
  print ch
  putStrLn $ "\ttestNd stateset size: " ++ show size
  (maybe
    (putStrLn "True")
    (\t -> putStrLn "False. A counter example is,"
      >> putStrLn (showBTree t))
    mt)
  where
  (P.EAlphabet ia) = m Map.! iav
  (P.EAlphabet oa) = m Map.! oav
  (P.ENd ind) = m Map.! indv
  (P.ENd ond) = m Map.! ondv
  (P.ETdtt tdtt) = m Map.! tdttv
  (size, mt) = testTypeCheck ia oa ind ond tdtt

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
  (memo, qs) = Nd.isEmptyWithQneFollow testNd (Nd.getFs testNd) Map.empty
