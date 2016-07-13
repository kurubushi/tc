{-#LANGUAGE TupleSections #-}

module Ta.Nd.Utils where

import Atom.Types
import Ta.Nd.Types
import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Set.Utils as S
import qualified Data.Map as Map

complement :: StateSet s => Nd s -> Nd s
complement = undefined
--complement nd = Nd {
--    getQs    = getQs nd
--  , getIs    = S.difference (getQs nd) (getIs nd)
--  , getFs    = getFs nd
--  , getTrans = getTrans nd
--}


intersection :: StateSet s => Nd s -> Nd s -> Nd s
intersection = undefined
--intersection nd1 nd2 = Nd {
--    getQs = S.cartesian (getQs nd1) (getQs nd2)
--  , getIs = S.cartesian (getIs nd1) (getIs nd2)
--  , getFs = S.cartesian (getFs nd1) (getFs nd2)
--  , getTrans = Map.fromList
--      . map (\((q,p),a) -> (((q,p),a),) 
--        . S.map (Expr . mix)
--        $ S.cartesian (relA (q,a) nd1) (relA (p,a) nd2))
--      . S.toList
--      . S.cartesian qs $ as
--}
--  where
--    qs = S.cartesian (getQs nd1) (getQs nd2)
--    as = S.map snd keys1 `S.intersection` S.map snd keys2
--    keys1 = S.fromList . Map.keys . getTrans $ nd1
--    keys2 = S.fromList . Map.keys . getTrans $ nd2
--    relA :: (Alphabet a, Q q, StateSet s) => (q,a) -> Nd a q s -> s (q,q)
--    relA key = S.map (\(Expr qpair) -> qpair)
--      . Map.findWithDefault S.empty key
--      . getTrans
--    mix :: ((a,a),(b,b)) -> ((a,b),(a,b))
--    mix ((x1,x2),(y1,y2)) = ((x1,y1),(x2,y2))


isEmpty :: (StateSet s, Eq (s Q)) => Nd s -> Bool
isEmpty = undefined
--isEmpty nd = isEmptyWithQne nd $ getFs nd

isEmptyWithQne :: (StateSet s, Eq (s Q)) => Nd s -> s Q -> Bool
isEmptyWithQne = undefined
--isEmptyWithQne nd qne
--  | qne == qne' = S.null (qne `S.intersection` getIs nd)
--  | otherwise   = isEmptyWithQne nd qne'
--  where
--    qne' = qne `S.union` newQs
--    newQs = S.fromList
--      . map fst
--      . Map.keys
--      . Map.filter (S.notNull . S.filter 
--          (\(Expr (q1,q2)) -> (q1 `S.member` qne) && q2 `S.member` qne))
--      . getTrans $ nd
