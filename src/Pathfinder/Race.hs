{-# LANGUAGE RankNTypes, DeriveDataTypeable, StandaloneDeriving, KindSignatures, FlexibleInstances #-}
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
import Data.Functor.Identity

type RandT = Rand.RandT

-- deriving instance Typeable (Character Identity)

data Race
  = Race
  { raceSize :: Size
  , raceAdjust :: Rand.MonadRandom m => Character -> m Character
  }
-- deriving (Data, Typeable)

{--
raceAdjust :: Monad m =>
  Race -> Character m -> RandT g m (Character m)
raceAdjust _race _chr = return _chr -- error "raceAdjust: not implemented yet"

elfAdjust :: Monad m => Kleisli (RandT g m) (Character m) (Character m)
elfAdjust = undefined

test :: Monad m => Kleisli (RandT g m) (Character m) (Character m)
test = elfAdjust >>> elfAdjust

--}
{-
addRacialTrait :: Monad m =>
  Text -> Text -> (Character -> m Character) -> Decorator Character m
addRacialTrait id dsc f =
  Decorator { ident = id
            , desc = dsc
            , decorator = f >=> chrAddRacialTrait (id, desc)
            }

addRacialTraits :: Monad m => [(Text, Text, Character -> m Character)] -> Decorator Character m
addRacialTraits lst = map (\un deux trois ->
                      Decorator { ident = un, decorator = trois >=> chrAddRacialTruc (un, deux), desc = Just deux })

{# "+2 blablabla" # "Les elfes sont agiles." # fun #}
{# "Immunités elfes" # "Les elfes sont immunes" #}
( "+2 blablabla", "Les elfes sont agiles.", fun )
( "Immunités elfes", "les elfes sont immunes", return )

elfAdjust :: CharacterM ()
elfAdjust =
  decorate $
  addRacialTraits
  [ ("+2 blablabla", "Les elfes sont agiles.", updateblablabla) :: (Text, Text, Monad m => Character -> m Character)
  ]
("Les elfes sont agiles, tant physiquement que mentalement, mais plutôt frêles", updateChrAbility (+2) DEX >>> updateChrAbility (+2) INT >>> updateChrAbility ((-)2) CON)
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
    --}-}
