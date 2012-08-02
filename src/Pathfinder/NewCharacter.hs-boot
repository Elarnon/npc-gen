module Pathfinder.NewCharacter where

import Utils
import Data.Dependent.Map (DMap)

data CKey a

type Character = Decorated (DMap CKey)
