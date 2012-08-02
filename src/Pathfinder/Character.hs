{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Pathfinder.Character where
{-
  ( Character (..)
  , chrLevel
  , chrUpdateAbility
  , chrAbilityModifier
  ) where

import Pathfinder.Misc
import Pathfinder.Class
import Pathfinder.Abilities
import Pathfinder.Skill
import Pathfinder.Feat
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Maybe
import Data.List
import Utils

type Text = T.Text
type Set = Set.Set
type Map = Map.Map

data Character = Character
  { name :: Text
  --, type :: Type
  --, subTypes :: [SubType]
  , size :: Size
  , sex :: Sex
  , alignment :: Alignment
  , chrFavCls :: Class
  , hp :: Integer
  , chrClasses :: Map Class ClassInstance
  , chrLvlAdj :: Integer
  , chrAbilities :: Abilities
  , chrSkills :: Map Skill SkillInstance
  , chrFeats :: Set Feat
  , langs :: Set Language
  }

-- type MyMonad = MonadRandom m => DecoratorT Character m

chrLevel :: Character -> Integer
chrLevel chr =
  Map.fold
    (\ci v -> v + clsLevel ci)
    (chrLvlAdj chr)
    (chrClasses chr)

chrUpdateAbility :: (Integer -> Integer) -> Ability -> Character -> Character
chrUpdateAbility f a chr =
  chr { chrAbilities = abUpdateAbility f a $ chrAbilities chr }

chrAbilityModifier :: Ability -> Character -> Integer
chrAbilityModifier a = abModifier a . chrAbilities -}
