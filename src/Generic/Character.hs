{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, StandaloneDeriving #-}
module Generic.Character
  {-( CUpdatableKey
  , CKey
  )-}
  where

import Data.GADT.Compare
import Data.Dependent.Map
import Leibniz
import Utils
import {-# SOURCE #-} Generic.Class
import Generic.Feat
import Generic.Skill
import Generic.Abilities
import Generic.Misc
import {-# SOURCE #-} Generic.Race
import Data.Map (Map)

-- TODO: newtype
type Character = Decorated (DMap CKey)

-- Not sure if useful…
class Ord o => Dip t o | t -> o where
  dip :: t a -> o

-- CUpdatableKey == for archetypes :-) ; Character -> m Type
-- CKey == for character ; Character = Decorated (DMap CKey)
-- Archetype =
--  { DMap CUpdatableKey : updaters (class, blablabla)
--  CUpdatableKey a -> m a
--  ; DMap CKey : default values
--  ; Map DKey (Character -> Character) : decorators
--  }

data Forget t = forall a. Forget !(t a)

data Wrap f t a where
  Wrapped :: t a -> Wrap f t (f a)

instance GEq t => GEq (Wrap f t) where
  geq (Wrapped k) (Wrapped k') = fmap tyCon $ geq k k'

-- Updatable keys - i.e. keys we can ask archetypes
-- TODO: Maybe AKey would then be better ?
data CUpdatable a where
  UClass :: CUpdatable SomeClass
  USkill :: CUpdatable Skill
  UFeat :: CUpdatable Feat
  UAbility :: CUpdatable Ability
  UFavBonus :: SomeClass -> CUpdatable FavBonus

data Any t a where
  Any :: t -> Any t a

-- TODO: deriving GEq
instance GEq CUpdatable where
  geq UClass UClass = Just Refl
  geq USkill USkill = Just Refl
  geq UFeat  UFeat  = Just Refl
  geq UAbility UAbility = Just Refl
  geq _       _     = Nothing

-- TODO: deriving GCompare ?
instance Dip CUpdatable Int where
  dip UClass = 0
  dip USkill = 1
  dip UFeat = 2
  dip UAbility = 3
  dip _ = error "dip ! NIY"

-- Keys that constitute a character
data CKey a where
  CLife :: CKey Integer
  CRace :: CKey Race
  CFavCls :: CKey SomeClass
  CClasses :: CKey (Map SomeClass ClassInstance)
  CSkills :: CKey (Map Skill SkillInstance)
  CAbility :: Ability -> CKey Integer
  CBAB :: CKey Integer
  CFort :: CKey Integer
  CWill :: CKey Integer
  CRefl :: CKey Integer

-- TODO: deriving GEq
instance GEq CKey where
  geq CLife CLife = Just Refl
  geq CRace CRace = Just Refl
  geq _     _     = Nothing

-- TODO: deriving GCompare ?
instance Dip CKey Int where
  dip CRace = 0
  dip CLife = 1
  dip _ = error "dip! NIY"

-- Wrapper to make UpdatableKeys compatibles with DSum and DMap
data WrapUpdate c m a where
  WCUK :: CUpdatable a -> WrapUpdate c m (c -> m a)

instance GEq (WrapUpdate c m) where
  geq (WCUK uk) (WCUK uk') = fmap (tyCon . tyCon) $ geq uk uk'

-- Automatic (undecidable :-( ) GCompare instance
instance (GEq t, Dip t o) => GCompare t where
  gcompare x x' =
    case geq x x' of
      Just Refl -> GEQ
      Nothing   -> case compare (dip x) (dip x') of
        EQ -> error "Programmer failure"
        LT -> GLT
        GT -> GGT
