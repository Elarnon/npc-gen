module Pathfinder.Misc
  ( Sex (..)
  , Size (..)
  , Alignment (..)
  , Language (..)
  , FavBonus (..)
  ) where

import Data.Text (Text)

data Sex = Male | Female
  deriving (Eq, Show)

data Size = ES | S | M | B | EB
  deriving (Eq, Show)

data Alignment
  = LB
  | LM
  | LN
  | NB
  | N
  | NM
  | CB
  | CN
  | CM
  deriving (Eq, Show)

newtype Language = Language Text
  deriving (Eq, Show)

data FavBonus = BonusHp | BonusSkill
