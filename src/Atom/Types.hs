{-#LANGUAGE GADTs #-}

module Atom.Types where

import Set.Types (StateSet)
import qualified Set.Types as S
import qualified Data.Map as Map

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

convertQs :: (StateSet s, Ord a) => s a -> s Q
convertQs qs = S.map makeQ $ S.fromList [0..(S.size qs)]

convertMap :: (StateSet s, Ord a) => s a -> (a -> Maybe Q)
convertMap qs = fmap makeQ
  . flip Map.lookup (Map.fromList . zip (S.toList qs) $ [0..])

unsafeConvertMap :: (StateSet s, Ord a) => s a -> (a -> Q)
unsafeConvertMap qs = makeQ -- q belongs to qs
  . (Map.!) (Map.fromList . zip (S.toList qs) $ [0..])

takeNewQ :: StateSet s => s Q -> Q
takeNewQ = makeQ . S.size


data BTree a where
  BTNode :: a -> BTree a -> BTree a -> BTree a
  BTEnd :: BTree a
