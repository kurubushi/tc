{-#LANGUAGE GADTs #-}

module Atom.Types where

import qualified Data.Set as S
import Data.Semigroup

data Alphabet = Alphabet Int | AlphabetEnd
  deriving (Eq, Ord, Show)

isEnd :: Alphabet -> Bool
isEnd AlphabetEnd = True
isEnd _           = False

isNotEnd :: Alphabet -> Bool
isNotEnd = not . isEnd


data Q = Q {
    qid   :: Int -- count up from 0
  , qsize :: Int -- qsize = length of Data.Set Q = max + 1
} deriving (Eq, Ord, Show)

instance Semigroup Q where
  q1 <> q2 = makeQ newSize newQ
    where newSize = qsize q1 * qsize q2
          newQ    = qid q2 * qsize q1 + qid q2

makeQ :: Int -> Int -> Q
makeQ n q = Q {qid = q, qsize = n} -- n > q


data BTree a where
  BTNode :: a -> BTree a -> BTree a -> BTree a
  BTEnd :: BTree a
