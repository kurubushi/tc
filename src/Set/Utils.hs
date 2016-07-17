{-#LANGUAGE FlexibleContexts #-}

module Set.Utils where

import Set.Types (StateSet)
import qualified Set.Types as S
import Data.Map.Lazy (Map)
<<<<<<< ca8e13e6b390ad6e44f54a670ea2dc56f04a5a10
import qualified Data.Map.Lazy as Map
=======
import qualified Data.Map as Map
>>>>>>> fix: wrong Map library name

toMap :: (StateSet s, Ord k) => s (k,v) -> Map k v
toMap = Map.fromList . S.toList

toMapfoldr :: (StateSet s, Ord k) =>
  (v' -> v' -> v') -> (v -> v') -> s (k,v) -> Map k v'
toMapfoldr f g = Map.fromListWith f . map (fmap g) . S.toList

fromMap :: (StateSet s, Ord (k,v)) => Map k v -> s (k,v)
fromMap = S.fromList . Map.toAscList
