{-#LANGUAGE FlexibleContexts #-}

module Set.Utils where

import Set.Types (StateSet)
import qualified Set.Types as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

toMap :: (StateSet s, Ord k) => s (k,v) -> Map k v
toMap = Map.fromList . S.toList

toMapfoldr :: (StateSet s, Ord k) =>
  (v' -> v' -> v') -> (v -> v') -> s (k,v) -> Map k v'
toMapfoldr f g = Map.fromListWith f . map (fmap g) . S.toList

fromMap :: (StateSet s, Ord (k,v)) => Map k v -> s (k,v)
fromMap = S.fromList . Map.toAscList
