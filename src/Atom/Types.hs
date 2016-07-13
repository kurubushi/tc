{-#LANGUAGE GADTs #-}

module Atom.Types where

import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Data.Map as Map

data Alphabet = Alphabet Integer | AlphabetEnd
  deriving (Eq, Ord, Show)

isEnd :: Alphabet -> Bool
isEnd AlphabetEnd = True
isEnd _           = False

isNotEnd :: Alphabet -> Bool
isNotEnd = not . isEnd


data Q = Q Integer -- count up from 0
  deriving (Eq, Ord, Show)

makeQ :: Integral a => a -> Q
makeQ = Q . toInteger

convert :: Ord a => [a] -> (a -> Maybe Q)
convert as = fmap makeQ
  . flip Map.lookup (Map.fromList . zip as $ [0..])

unsafeConver :: Ord a => [a] -> (a -> Q)
unsafeConver as = makeQ -- a belongs to as
  . (Map.!) (Map.fromList . zip as $ [0..])

takeNewQ :: StateSet s => s Q -> Q
takeNewQ = makeQ . S.size


data BTree a where
  BTNode :: a -> BTree a -> BTree a -> BTree a
  BTEnd :: BTree a
