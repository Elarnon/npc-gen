{-# LANGUAGE GADTs, Rank2Types #-}
module Pathfinder.Archetype
  ( Archetype (..)
  , ArchTag
  ) where

import Data.Dependent.Map (DMap)
import Control.Monad.Random
import Pathfinder.NewCharacter
import Pathfinder.Abilities
import Pathfinder.Misc

data Archetype m = Archetype
  { archPickValue :: ArchTag t => t a -> DMap CKey -> m (Maybe a) }

class ArchTag t where
  aTag :: t a -> ATag t a

data ATag t a where
  UCUpdatable :: CUpdatable a -> ATag CUpdatable a
  UCKey       :: CKey a -> ATag CKey a

instance ArchTag CKey where
  aTag = UCKey

instance ArchTag CUpdatable where
  aTag = UCUpdatable

_bladeWarriorArchetype :: (MonadRandom m, ArchTag t) => t a -> DMap CKey -> m (Maybe a)
_bladeWarriorArchetype t _chr =
  case aTag t of
    UCUpdatable upd ->
      case upd of
      UClass -> return . Just $ undefined --Warrior
      USkill -> error "Warrior/USkill"
      UFeat  -> error "Warrior/UFeat" -- firstAvailableInList ...
      UAbility -> return . Just $ STR
      UFavBonus _ -> return . Just $ BonusSkill
    UCKey uck -> undefined
      {-case uck of
        _ -> undefined-}

bladeWarriorArchetype :: MonadRandom m => Archetype m
bladeWarriorArchetype = Archetype _bladeWarriorArchetype
