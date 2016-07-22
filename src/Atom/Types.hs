{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE TypeFamilies #-}

module Atom.Types where

import Set.Types (StateSet)
import qualified Set.Types as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Control.Monad.Free (Free(..))
import Data.Void (Void)

data Alphabet = Alphabet Integer | AlphabetEnd
  deriving (Eq, Ord, Show)

makeAlphabet :: Integral a => a -> Alphabet
makeAlphabet = Alphabet . toInteger

endAlphabet :: Alphabet
endAlphabet = AlphabetEnd

isEnd :: Alphabet -> Bool
isEnd AlphabetEnd = True
isEnd _           = False

isNotEnd :: Alphabet -> Bool
isNotEnd = not . isEnd


class Ord q => Q q where
  type QElem q
  makeQ :: QElem q -> q
  convertMap :: StateSet s => s q -> (q -> Maybe QInt)
  unsafeConvertMap :: StateSet s => s q -> (q -> QInt)
  takeNewQ :: StateSet s => s q -> q

data QD q = QD q
          | NewQ Integer -- count up from 0
deriving instance Eq q => Eq (QD q)
deriving instance Ord q => Ord (QD q)
deriving instance Show q => Show (QD q)

type QInt = QD Int

instance Ord a => Q (QD a) where
  type QElem (QD a) = a
  makeQ = QD
  convertMap qs q =fmap makeQ
    . Map.lookup q
    . Map.fromList
    . zip (S.toList qs) $ [0..]
  unsafeConvertMap qs q = makeQ -- ** q must belong to qs **
    . (Map.! q) -- unsafe lookup function
    . Map.fromList
    . zip (S.toList qs) $ [0..]
  takeNewQ = unsafeSuccQ . maxDummyQ
    where
      unsafeSuccQ (NewQ n) = NewQ (n+1)
      maxDummyQ qs = let ds = S.toList . S.filter isDummy $ qs in
        if null ds then NewQ 0 else maximum ds
      isDummy (NewQ _) = True
      isDummy _        = False
 

data BTree' a x where
  BTEnd'  :: BTree' a x
  BTNode' :: a -> x -> x -> BTree' a x
  deriving (Eq, Ord, Show)

type BTree a = Free (BTree' a) Void

btEnd :: BTree a
btEnd = Free BTEnd'

makeNode :: a -> BTree a -> BTree a -> BTree a
makeNode x t1 t2 = Free $ BTNode' x t1 t2

isBTEnd :: BTree a -> Bool
isBTEnd (Free BTEnd') = True
isBTEnd _             = False

isNotBTEnd :: BTree a -> Bool
isNotBTEnd = not . isBTEnd

getElems :: BTree a -> Maybe (a, BTree a, BTree a)
getElems (Free (BTNode' x t1 t2)) = Just (x, t1, t2)
getElems _                        = Nothing

-- tree must not be End.
unsafeGetElems :: BTree a -> (a, BTree a, BTree a)
unsafeGetElems (Free (BTNode' x t1 t2)) = (x, t1, t2)
