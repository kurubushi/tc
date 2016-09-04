{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}

module Ta.Nd.Types where

import Atom.Types
import Data.Map.Lazy (Map)

type Expr q = (q, q)

type Trans q s = Expr q -> Alphabet -> s q

data Nd q s = Nd {
    getQs    :: s q
  , getIs    :: s q
  , getFs    :: s q
  , getTrans :: Trans q s
}

type FollowMemoQ q = Map q (Alphabet,Expr q)
