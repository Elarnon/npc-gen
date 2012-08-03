{-# LANGUAGE OverloadedStrings #-}
module Generic.Skill
  ( Skill (..)
  , SkillInstance (..)
  , skAdd
  , makeInstance
  ) where

import Generic.Abilities
import Data.Text (Text)

-- The datatype for a skill. Nothing to see here, really.
data Skill = Skill
  { skName :: Text
  , requireTraining :: Bool
  , ability :: Ability
  }
  deriving (Eq, Show, Ord)

-- The instance associated with a skill
-- bonus should now be computed on the fly
data SkillInstance = SkillInstance
  { ranks :: Integer
  -- , bonus :: Integer
  }
  deriving (Eq, Show)

makeInstance :: Integer -> SkillInstance
makeInstance i = SkillInstance { ranks = i {-, bonus = 0-} }

skAdd :: SkillInstance -> SkillInstance -> SkillInstance
skAdd sk1 sk2 =
  SkillInstance { ranks = ranks sk1 + ranks sk2{-, bonus = bonus sk1 + bonus sk2-} }
