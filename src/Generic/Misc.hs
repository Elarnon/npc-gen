module Generic.Misc
  ( Sex (..)
  , Size (..)
  , Alignment (..)
  , Language (..)
  , FavBonus (..)
  , convertFavBonus
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

-- TODO: maybe we should have a 'RealFavBonus' thing
-- or even better, an opaque FavBonus with bonusHp,
-- bonusSkill, noBonus as variables, and getBonusHp and getBonusSkill
-- as getters
convertFavBonus :: FavBonus -> (Integer, Integer)
convertFavBonus BonusHp = (0, 1)
convertFavBonus BonusSkill = (1, 0)
