{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Pathfinder.Abilities
  ( Abilities
  , Ability (..)
  , setAbility
  , getAbility
  , updateAbility
  , defaultAbilities
  , fromDefault
  ) where

import Data.Maybe
import Data.Functor.Identity

data Abilities m = Abilities
  { str :: m Integer
  , dex :: m Integer
  , con :: m Integer
  , int :: m Integer
  , wis :: m Integer
  , cha :: m Integer
  }

deriving instance Show a => Show (Identity a)
deriving instance Show (Abilities Identity)

data Ability = STR | DEX | CON | INT | WIS | CHA
  deriving (Eq, Show, Ord)

defaultAbilities :: Monad m => Abilities m
defaultAbilities = Abilities
  { str = return 10
  , dex = return 10
  , int = return 10
  , con = return 10
  , wis = return 10
  , cha = return 10
  }

fromDefault :: Monad m => Abilities m -> Abilities Maybe -> Abilities m
fromDefault mAbi mayAbi =
  mAbi {
    str = maybe (str mAbi) return (str mayAbi),
    dex = maybe (dex mAbi) return (dex mayAbi),
    int = maybe (int mAbi) return (int mayAbi),
    con = maybe (con mAbi) return (con mayAbi),
    wis = maybe (wis mAbi) return (wis mayAbi),
    cha = maybe (cha mAbi) return (cha mayAbi)
  }

setAbility :: Monad m => Ability -> m Integer -> Abilities m -> Abilities m
setAbility STR = \i a -> a { str = i }
setAbility DEX = \i a -> a { dex = i }
setAbility CON = \i a -> a { con = i }
setAbility INT = \i a -> a { int = i }
setAbility WIS = \i a -> a { wis = i }
setAbility CHA = \i a -> a { cha = i }

getAbility :: Ability -> Abilities m -> m Integer
getAbility STR = str
getAbility DEX = dex
getAbility CON = con
getAbility INT = int
getAbility WIS = wis
getAbility CHA = cha

updateAbility :: Monad m =>
  (Integer -> Integer) -> Ability -> Abilities m -> Abilities m
updateAbility f ab abs =
  let val = getAbility ab abs in
  setAbility ab (val >>= \val -> return $ f val) abs
