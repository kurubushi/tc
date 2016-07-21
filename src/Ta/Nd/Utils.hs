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
import Control.Monad
import Control.Applicative
import Control.Arrow

complete :: (StateSet s, Q q) => s Alphabet -> Nd q s -> Nd q s
complete as nd
  | S.null unDefinesInOriginal = nd
  | otherwise = Nd {
      getQs = qs
    , getIs = getIs nd
    , getFs = getFs nd
    , getTrans = getTrans nd `Map.union` addedTrans
  }
  where
    dummyQ = takeNewQ (getQs nd)
    qs = getQs nd `S.union` S.fromList [dummyQ]
    addedTrans = S.toMapfoldr S.union (\e -> S.fromList [e]) . S.map (\(a,e) -> ((dummyQ,a), e)) $ unDefines
    unDefines = allCombis `S.difference` defines
    unDefinesInOriginal = S.filter (\(a,Expr (q1,q2)) -> q1/=dummyQ && q2 /=dummyQ) unDefines
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
  Nd q s -> s q -> FollowMemoQ q -> (FollowMemoQ q, s q)
isEmptyWithQneFollow nd qne memo
  | qne == qne' || S.notNull reachedQs = (memo', reachedQs)
  | otherwise = isEmptyWithQneFollow nd qne' memo'
  where
    reachedQs = qne' `S.intersection` getIs nd
    makeKeysSet = S.fromList . map fst . Map.keys
    -- 前のものがあれば更新しない
    notUpdateFromList memo = Map.unionWith const memo . Map.fromListWith const
    takeSample = (\(Expr (q1,q2)) -> (q1,q2)) . head . S.toList
    makeMemo = notUpdateFromList memo
      . map (\((q,a),es) -> (q, (a,takeSample es)))
      . Map.toList
    (qne', memo') = (makeKeysSet &&& makeMemo)
      . Map.filter S.notNull
      . Map.map (S.filter (\(Expr (q1,q2)) ->
          q1 `S.member` qne && q2 `S.member` qne))
      . getTrans $ nd


sampleCounterExample :: (StateSet s, Q q) =>
  Nd q s -> FollowMemoQ q -> q -> Maybe (BTree Alphabet)
sampleCounterExample nd memo q
  | q `S.member` getFs nd = Just btEnd
  | otherwise = makeTree <=< Map.lookup q $ memo
  where
    makeTree (a,(q1,q2)) = makeNode a
      <$> sampleCounterExample nd memo q1
      <*> sampleCounterExample nd memo q2

-- memo and q must be created by isEmptyWithQneFollow.
-- isEmptyWithQneFollowで得られたものならば必ず成功するはず
unsafeSampleCounterExample :: (StateSet s, Q q) =>
  Nd q s -> FollowMemoQ q -> q -> BTree Alphabet
unsafeSampleCounterExample nd memo q
  | q `S.member` getFs nd = btEnd
  | otherwise = makeTree . (Map.! q) $ memo
  where
    makeTree (a,(q1,q2)) = makeNode a
      (unsafeSampleCounterExample nd memo q1)
      (unsafeSampleCounterExample nd memo q2)
