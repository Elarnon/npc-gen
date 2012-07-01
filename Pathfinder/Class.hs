module Pathfinder.Class
  ( Class (..)
  , ClassInstance 
  , newInstance
  , lvl
  , defaultClasses
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map

type Map = Map.Map
type Text = T.Text

data Class = Class
  { clsName :: Text
  , clsSkills :: Integer
  , life :: Integer
  , getHpDice :: Integer
  }
  deriving (Eq, Show, Ord)

data ClassInstance = ClassInstance
  { lvl :: Integer
  }
  deriving (Eq, Show)

newInstance :: Integer -> ClassInstance
newInstance i = ClassInstance { lvl = i }

defaultClasses :: Map Class ClassInstance
defaultClasses = Map.empty
