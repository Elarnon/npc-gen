{-# LANGUAGE GADTs, RankNTypes #-}
module Pathfinder.Class
  ( Class
  , SomeClass
  , clsName , clsHpDice, clsSkills
  , ClassInstance (..)
  , IncClass (..)
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
import Control.Monad.Random
import {-# SOURCE #-} Pathfinder.Archetype
import {-# SOURCE #-} Pathfinder.NewCharacter

-- A class representing classes (oh yeah)
class Class c where
  clsName :: c -> Text
  clsHpDice :: c -> Integer
  clsSkills :: c -> Integer
  -- Function to increase anything that a class can increase
  -- See the IncClass class
  incClass :: c -> IncClass a -> Integer -> a

-- Existential wrapper around a class. This more or less allow to define
-- more class by creating a type plus a Class instance, which I'm not sure
-- is really better than a record but whatever.
data SomeClass = forall c. Class c => SomeClass c

instance Eq SomeClass where
  x == y = clsName x == clsName y

instance Ord SomeClass where
  compare x y = compare (clsName x) (clsName y)

instance Class SomeClass where
  clsName (SomeClass c) = clsName c
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

-- Data that we can ask a class to update
data IncClass a where
  ICBAB     :: IncClass Integer
  ICFort    :: IncClass Integer
  ICWill    :: IncClass Integer
  ICRefl    :: IncClass Integer
  ICSpecial :: MonadRandom m => IncClass (Archetype m -> Character -> m (Character))

icBAB = ICBAB
icFort = ICFort
icWill = ICWill
icRefl = ICRefl
