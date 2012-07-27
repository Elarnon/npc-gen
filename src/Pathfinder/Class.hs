module Pathfinder.Class
  ( Class (..)
  , ClassInstance (..)
  , clsAddLevels
  , defaultClasses
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map

type Map = Map.Map
type Text = T.Text

data Class = Class
  { clsName :: Text
  , clsSkills :: Integer
  , clsLife :: Integer
  , clsHpDice :: Integer
  }
  deriving (Eq, Show, Ord)

data ClassInstance = ClassInstance
  { clsLevel :: Integer
  }
  deriving (Eq, Show)

clsAddLevels :: ClassInstance -> ClassInstance -> ClassInstance
clsAddLevels c c' = ClassInstance { clsLevel = clsLevel c + clsLevel c' }

defaultClasses :: Map Class ClassInstance
defaultClasses = Map.empty
