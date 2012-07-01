module Pathfinder.Character
  ( Character (..)
  , getCharacterLevel
  ) where

import Pathfinder.Misc
import {-# SOURCE #-} Pathfinder.Race
import Pathfinder.Class
import Pathfinder.Abilities
import Pathfinder.Skill
import Pathfinder.Feat
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map

type Text = T.Text
type Set = Set.Set
type Map = Map.Map

data Character m = Character
  { name :: m Text
  , race :: m Race
  , size :: m Size
  , sex :: m Sex
  , alignment :: m Alignment
  , favouredClass :: m Class
  , hp :: m Integer
  , classes :: m (Map Class ClassInstance)
  , lvlAdjust :: m Integer
  , abilities :: Abilities m
  , skills :: m (Map Skill SkillInstance)
  , feats :: m (Set Feat)
  , lang :: m (Set Language)
  }

getCharacterLevel :: Monad m => Character m -> m Integer
getCharacterLevel chr = 
  lvlAdjust chr >>= \adj ->
  classes chr >>= \mapClasses ->
  return $ Map.fold (\ci v -> v + lvl ci) adj mapClasses
