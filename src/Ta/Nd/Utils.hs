module Ta.Nd.Utils where

import Atom.Types
import Ta.Nd.Types
import Set.Types (StateSet)
import qualified Set.Types as S

complement :: (Alphabet a, Q q, StateSet s) => Nd a q s -> Nd a q s
complement nd = Nd {
    getQs    = getQs nd
  , getIs    = S.difference (getQs nd) (getIs nd)
  , getFs    = getFs nd
  , getTrans = getTrans nd
}
