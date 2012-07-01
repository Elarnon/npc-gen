{-# LANGUAGE RankNTypes, DeriveDataTypeable, StandaloneDeriving, KindSignatures #-}
module Pathfinder.Race
  ( Race 
  , raceSize
  , raceAdjust
  ) where

import Pathfinder.Misc
import Pathfinder.Character
import qualified Control.Monad.Random as Rand
import Control.Arrow
import Data.Data
import Data.Dynamic

type RandT = Rand.RandT

data Race
  = Race
  { raceSize :: Size
  , raceAdjust :: Monad m => Character m -> RandT g m (Character m)
  }

{--
raceAdjust :: Monad m =>
  Race -> Character m -> RandT g m (Character m)
raceAdjust _race _chr = return _chr -- error "raceAdjust: not implemented yet"

elfAdjust :: Monad m => Kleisli (RandT g m) (Character m) (Character m)
elfAdjust = undefined

test :: Monad m => Kleisli (RandT g m) (Character m) (Character m)
test = elfAdjust >>> elfAdjust

--}
{--
elfAdjust :: Monad m => Character m -> RandT g m (Character m)
elfAdjust =
  setChrSize M >>>
  setBaseSpeed 9 >>>
  addRacialFeats Elf
    [ ("Abilities", updateChrAbility (+2) DEX >>>
                    updateChrAbility (+2) INT >>>
                    updateChrAbility ((-)2) CON)
    , ("Low-light vision", "Elves can see twice as far as humans in conditions of dim light.")
    , ("Elven immunities", "Blablabla")
    , ("Elven magic", "Blablabla")
    , ("Keen senses", addSkillBonus (+2) Perception)
    , ("Weapon familiarity", weaponsProficiency [LongBows, CompositeLongBows, ...])
    , ("Languages", addLanguages [Common, Elven] >>> addAvailableLanguages [Celestial, ...])
    --}
