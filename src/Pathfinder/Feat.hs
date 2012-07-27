module Pathfinder.Feat
  ( Feat (..)
  , defaultFeats
  ) where

import qualified Data.Set as Set

type Set = Set.Set

newtype Feat = Feat String
  deriving (Eq, Show, Ord)

defaultFeats :: Set Feat
defaultFeats = Set.empty
