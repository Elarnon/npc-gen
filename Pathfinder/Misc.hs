{-# LANGUAGE OverloadedStrings #-}

module Pathfinder.Misc
  ( Sex (..)
  , Size (..)
  , Alignment (..)
  , Language (..)
  , defaultLanguages
  , defaultName
  , defaultHp
  , defaultLvlAdjust
  ) where

import qualified Data.Text as T
import qualified Data.Set as Set

type Text = T.Text
type Set = Set.Set

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

defaultLanguages :: Set Language
defaultLanguages = Set.empty

defaultName :: Text
defaultName = "An unknown hero"

defaultHp :: Integer
defaultHp = 0

defaultLvlAdjust :: Integer
defaultLvlAdjust = 0
