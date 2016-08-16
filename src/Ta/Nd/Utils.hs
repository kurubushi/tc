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
import Data.Maybe (fromMaybe)

complete :: (StateSet s, Q q) => s Alphabet -> Nd q s -> Nd q s
complete as nd
  | S.null unDefinesInOriginal = nd
  | otherwise = Nd {
      getQs = qs
    , getIs = getIs nd
    , getFs = getFs nd
    , getTrans = tr
  }
  where
    dummyQ = takeNewQ (getQs nd)
    qs = getQs nd `S.union` S.fromList [dummyQ]
    tr (q1,q2) a
      | q1==dummyQ || q2==dummyQ = S.fromList [dummyQ]
      | otherwise = (getTrans nd) (q1,q2) a
    unDefinesInOriginal = S.filter (\(a,(q1,q2)) -> q1/=dummyQ && q2 /=dummyQ) unDefines
    unDefines = -- :: s (Alphabet,Expr)
      S.filter (\(a,e) -> S.null $ (getTrans nd) e a)
      $ as `S.cartesian` (qs `S.cartesian` qs)


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
  , getTrans = \(q1,q2) a ->
      fromMaybe S.empty $ trans' <$> getQ q1 <*> getQ q2 <*> pure a

}
  where
    conv = makeQ
    qs = S.cartesian (getQs nd1) (getQs nd2)
    is = S.cartesian (getIs nd1) (getIs nd2)
    fs = S.cartesian (getFs nd1) (getFs nd2)
    trans' (q11,q12) (q21,q22) a =
      S.map conv $ tr1 (q11,q21) a `S.cartesian` tr2 (q12,q22) a

    tr1 = getTrans nd1
    tr2 = getTrans nd2


isEmpty :: (StateSet s, Q q, Eq (s q)) => s Alphabet -> Nd q s -> Bool
isEmpty as nd = isEmptyWithQne as nd $ getFs nd

isEmptyWithQne :: (StateSet s, Q q, Eq (s q)) => s Alphabet -> Nd q s -> s q -> Bool
isEmptyWithQne as nd qne = S.null . snd $ isEmptyWithQneFollow as nd qne Map.empty

isEmptyWithQneFollow :: (StateSet s, Q q, Eq (s q)) =>
  s Alphabet -> Nd q s -> s q -> FollowMemoQ q -> (FollowMemoQ q, s q)
isEmptyWithQneFollow as nd qne memo
  | qne == qne' || S.notNull reachedQs = (memo', reachedQs)
  | otherwise = isEmptyWithQneFollow as nd qne' memo'
  where
    reachedQs = qne' `S.intersection` getIs nd
    add = Map.unionWith const
    makeQne = S.foldr (\(a,e) acc -> acc `S.union` trans e a) S.empty
    makeMemo = S.foldr (\(a,e) acc -> acc `add` makeMemoSub a e) memo
    makeMemoSub a e = S.foldr (\q acc -> acc `add` Map.singleton q (a,e)) Map.empty (trans e a)
    (qne', memo') = (makeQne &&& makeMemo)
      $ as `S.cartesian` (qne `S.cartesian` qne)
    trans = getTrans nd


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
