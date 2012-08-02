{-# LANGUAGE GADTs, RankNTypes #-}
module Pathfinder.Class
  ( Class
  , SomeClass
  , clsName , clsLife , clsHpDice, clsSkills
  , ClassInstance (..)
  , icBAB, icFort, icWill, icRefl
  , incClass
  , clsAddLevels
  , defaultClasses
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dependent.Map (DMap)
import {-# SOURCE #-} Pathfinder.NewCharacter

-- TODO FIXME
{-
data Class = Class
  { clsName :: Text
  , clsSkills :: Integer
  , clsLife :: Integer
  , clsHpDice :: Integer
  , incClass :: forall a. IncClass a -> Integer -> a
  }

instance Show Class where
  show = show . clsName

instance Eq Class where
  c == c' = clsName c == clsName c'

instance Ord Class where
  compare c c' = compare (clsName c) (clsName c')
-}

class Class c where
  -- data ClassSpecial c :: *
  clsName :: c -> Text
  clsLife :: c -> Integer
  clsHpDice :: c -> Integer
  clsSkills :: c -> Integer
  incClass :: c -> IncClass a -> Integer -> a

data SomeClass = forall c. Class c => SomeClass c

instance Eq SomeClass where
  x == y = clsName x == clsName y

instance Ord SomeClass where
  compare x y = compare (clsName x) (clsName y)

instance Class SomeClass where
  clsName (SomeClass c) = clsName c
  clsLife (SomeClass c) = clsLife c
  clsHpDice (SomeClass c) = clsHpDice c
  clsSkills (SomeClass c) = clsSkills c
  incClass (SomeClass c) = incClass c

data ClassInstance = ClassInstance
  { clsLevel :: Integer
  }
  deriving (Eq, Show)

clsAddLevels :: ClassInstance -> ClassInstance -> ClassInstance
clsAddLevels c c' = ClassInstance { clsLevel = clsLevel c + clsLevel c' }

defaultClasses :: Map SomeClass ClassInstance
defaultClasses = Map.empty

data IncClass a where
  ICBAB     :: IncClass Integer
  ICFort    :: IncClass Integer
  ICWill    :: IncClass Integer
  ICRefl    :: IncClass Integer
  ICSpecial :: IncClass (Character -> Character)

icBAB = ICBAB
icFort = ICFort
icWill = ICWill
icRefl = ICRefl
icSpecial = ICSpecial
