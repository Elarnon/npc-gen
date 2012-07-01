module Pathfinder.Skill
  ( Skill (..)
  , SkillInstance
  , defaultSkills
  , makeInstance
  , addRank
  , bonus
  , ranks
  ) where

import Pathfinder.Abilities
import qualified Data.Text as T
import qualified Data.Map as Map

type Map = Map.Map
type Text = T.Text

data Skill = Skill
  { skName :: Text
  , requireTraining :: Bool
  , ability :: Ability
  }
  deriving (Eq, Show, Ord)

data SkillInstance = SkillInstance
  { ranks :: Integer
  , bonus :: Integer
  }
  deriving (Eq, Show)

defaultSkills :: Map Skill SkillInstance
defaultSkills = Map.empty

makeInstance :: Integer -> SkillInstance
makeInstance i = SkillInstance { ranks = i , bonus = 0 }

addRank :: SkillInstance -> SkillInstance -> SkillInstance
addRank sk1 sk2 = sk2 { ranks = ranks sk1 + ranks sk2 }
