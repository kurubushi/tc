{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeFamilies #-}

module Ta.Nd.Utils where

import Atom.Types
import Ta.Nd.Types
import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Set.Utils as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Control.Applicative (liftA2)

complete :: (StateSet s, Q q) => s Alphabet -> Nd q s -> Nd q s
complete as nd
  | S.null unDefines = nd
  | otherwise = Nd {
      getQs = qs
    , getIs = getIs nd
    , getFs = getFs nd
    , getTrans = getTrans nd `Map.union` addedTrans
  }
  where
    dummyQ = takeNewQ (getQs nd)
    qs = getQs nd `S.union` S.fromList [dummyQ]
    addedTrans = S.toMapfoldr S.union (\e -> S.fromList [e]). S.map (\(a,e) -> ((dummyQ,a), e)) $ unDefines
    unDefines = allCombis `S.difference` defines
    allCombis = as `S.cartesian` S.map Expr (qs `S.cartesian` qs)
    defines = -- :: s (Alphabet,Expr)
      Map.foldrWithKey (\(q,a) es acc -> S.map (a,) es `S.union` acc) S.empty
      . getTrans $ nd


complement :: (StateSet s, Q q) => s Alphabet -> Nd q s -> Nd q s
complement as nd = Nd {
    getQs    = getQs nd'
  , getIs    = S.difference (getQs nd') (getIs nd')
  , getFs    = getFs nd'
  , getTrans = getTrans nd'
}
  where nd' = complete as nd


intersection :: (StateSet s, Q q1, Q q2, Q q', QElem q' ~ (q1,q2)) =>
  Nd q1 s -> Nd q2 s -> Nd q' s
intersection nd1 nd2 = Nd {
    getQs = S.map conv qs
  , getIs = S.map conv is
  , getFs = S.map conv fs
  , getTrans = Map.fromList
      . map (\((q,p),a) -> ((conv (q,p),a),) 
        . S.map (Expr . convPair . mix)
        $ S.cartesian (relA (q,a) nd1) (relA (p,a) nd2))
      . S.toList
      . S.cartesian qs $ as
}
  where
    conv = makeQ
    qs = S.cartesian (getQs nd1) (getQs nd2)
    is = S.cartesian (getIs nd1) (getIs nd2)
    fs = S.cartesian (getFs nd1) (getFs nd2)
    as = S.map snd keys1 `S.intersection` S.map snd keys2
    keys1 = S.fromList . Map.keys . getTrans $ nd1
    keys2 = S.fromList . Map.keys . getTrans $ nd2
    relA :: (Ord q, StateSet s) => (q,Alphabet) -> Nd q s -> s (q,q)
    relA key = S.map (\(Expr qpair) -> qpair)
      . Map.findWithDefault S.empty key
      . getTrans
    mix :: ((a,a),(b,b)) -> ((a,b),(a,b))
    mix ((x1,x2),(y1,y2)) = ((x1,y1),(x2,y2))
    convPair (x,y) = (conv x, conv y)


isEmpty :: (StateSet s, Q q, Eq (s q)) => Nd q s -> Bool
isEmpty nd = isEmptyWithQne nd $ getFs nd

isEmptyWithQne :: (StateSet s, Q q, Eq (s q)) => Nd q s -> s q -> Bool
isEmptyWithQne nd qne = S.null . snd $ isEmptyWithQneFollow nd qne Map.empty

isEmptyWithQneFollow :: (StateSet s, Q q, Eq (s q)) =>
  Nd q s -> s q -> FollowMemoQ s q -> (FollowMemoQ s q, s q)
isEmptyWithQneFollow nd qne memo
  | qne == qne' = (memo', qne `S.intersection` getIs nd)
  | otherwise   = isEmptyWithQneFollow nd qne' memo'
  where
    qne' = qne `S.union` newQs
    makeKeysSet = S.fromList . map fst . Map.keys
    makeMemo = Map.fromListWith S.union
      . map (\((q,a),es) -> (q, S.map (\(Expr (q1,q2)) -> (a,(q1,q2))) es))
      . Map.toList
    (newQs, memo') = (\m -> (makeKeysSet m, makeMemo m))
      . Map.filter S.notNull
      . Map.map (S.filter (\(Expr (q1,q2)) ->
          q1 `S.member` qne && q2 `S.member` qne))
      . getTrans $ nd

unsafeSampleCounterExample :: (StateSet s, Q q) =>
  Nd q s -> FollowMemoQ s q -> q -> BTree Alphabet
unsafeSampleCounterExample nd memo q
  | q `S.member` getFs nd = btEnd
  | otherwise = makeTree . (Map.!) memo $ q
  where
    sample = head . S.toList
    makeTree follows = let (a,(q1,q2)) = sample follows in
      makeNode a
        (unsafeSampleCounterExample nd memo q1)
        (unsafeSampleCounterExample nd memo q2)
