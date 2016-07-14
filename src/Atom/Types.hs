{-#LANGUAGE GADTs #-}

module Atom.Types where

import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Data.Map as Map
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


data Q = Q Integer -- count up from 0
  deriving (Eq, Ord, Show)

makeQ :: Integral a => a -> Q
makeQ = Q . toInteger

convertMap :: (StateSet s, Ord a) => s a -> (a -> Maybe Q)
convertMap qs q =fmap makeQ
  . Map.lookup q
  . Map.fromList
  . zip (S.toList qs) $ [0..]

unsafeConvertMap :: (StateSet s, Ord a) => s a -> (a -> Q)
unsafeConvertMap qs q = makeQ -- ** q must belong to qs **
  . (Map.! q) -- unsafe lookup function
  . Map.fromList
  . zip (S.toList qs) $ [0..]

takeNewQ :: StateSet s => s Q -> Q
takeNewQ = makeQ . S.size -- (qs :: s Q) `S.union` (S.fromList [takeNewQ]) <= OK


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
isBTEnd _             = True

isNotBTEnd :: BTree a -> Bool
isNotBTEnd = not . isBTEnd

getElems :: BTree a -> Maybe (a, BTree a, BTree a)
getElems (Free (BTNode' x t1 t2)) = Just (x, t1, t2)
getElems _                        = Nothing

-- tree must not be End.
unsafeGetElems :: BTree a -> (a, BTree a, BTree a)
unsafeGetElems (Free (BTNode' x t1 t2)) = (x, t1, t2)
