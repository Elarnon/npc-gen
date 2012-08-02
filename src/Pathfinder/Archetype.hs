{-# LANGUAGE GADTs, Rank2Types #-}
module Pathfinder.Archetype
  ( Archetype (..)
  , ArchTag
  , combineArchs
  , pickN
  ) where

import Data.Dependent.Map (DMap)
import Control.Monad
import Control.Monad.Random
import Data.Maybe
import Pathfinder.NewCharacter
import Pathfinder.Abilities
import Pathfinder.Misc
import Utils

-- An archetypes basically responds to queries relatively to a
-- "raw" character (i.e., DMap CKey), that should probably (TODO ?) be a
-- "complete" character (i.e., Character), in some monad, and with the
-- ability to give no answer - Nothing
data Archetype m = Archetype
  { archPickValue :: ArchTag t => t a -> DMap CKey -> m (Maybe a) }

-- Implementation that should separate the chosen value from the rest of the
-- values
-- TODO: should be wrapped in a Maybe
randFromList :: MonadRandom m => [(a, Rational)] -> m (a, [(a, Rational)])
randFromList = error "randFromList NIY"

pickDefault :: (MonadRandom m, WithDefault a, ArchTag t) =>
  Archetype m -> t a -> DMap CKey -> m a
pickDefault arch k dmap = liftM maybeDefault $ archPickValue arch k dmap

-- Pick a value amongst a lot of archetypes
pick :: (MonadRandom m, ArchTag t) =>
  [(Archetype m, Rational)] -> t a -> DMap CKey -> m (Maybe a)
pick []         _ _    = return Nothing
pick archetypes k dmap =
  randFromList archetypes >>= \(archetype,  archs) ->
  archPickValue archetype k dmap >>= \val ->
  case val of
    Just x -> return $ Just x
    Nothing -> pick archs k dmap

combineArchs :: MonadRandom m => [(Archetype m, Rational)] -> Archetype m
combineArchs archs = Archetype $ pick archs

pickN :: (ArchTag t, MonadRandom m) =>
  Archetype m -> t a -> Integer -> DMap CKey -> m [a]
pickN arch k nb dmap =
  if nb <= 0
    then return []
    else liftM2 (++)
          (liftM maybeToList $ archPickValue arch k dmap)
          (pickN arch k (nb-1) dmap)

-- A class to define the kind of queries an archetype can answer. This 
-- really is pretty useless.
class ArchTag t where
  aTag :: t a -> ATag t a

data ATag t a where
  UCUpdatable :: CUpdatable a -> ATag CUpdatable a
  UCKey       :: CKey a -> ATag CKey a

instance ArchTag CKey where
  aTag = UCKey

instance ArchTag CUpdatable where
  aTag = UCUpdatable

-- This is just a test
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
