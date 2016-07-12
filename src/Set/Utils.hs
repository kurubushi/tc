{-#LANGUAGE FlexibleContexts #-}

module Set.Utils where

import Set.Types
import Data.Map (Map)
import qualified Data.Map as Map

toMap :: (StateSet s, Ord k) => s (k,v) -> Map k v
toMap = Map.fromList . toList

fromMap :: (StateSet s, Ord (k,v)) => Map k v -> s (k,v)
fromMap = fromList . Map.toAscList
