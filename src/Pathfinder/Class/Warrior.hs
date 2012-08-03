{-# LANGUAGE OverloadedStrings, GADTs #-}
module Pathfinder.Class.Warrior where

import Pathfinder.Class
import Pathfinder.Archetype
import Pathfinder.NewCharacter
import Control.Monad.Random

data Warrior = Warrior

instance Class Warrior where
  clsName Warrior = "Warrior"
  clsHpDice Warrior = 10 -- TODO
  clsSkills Warrior = 10 -- TODO
  incClass Warrior c =
    case c of
      ICBAB -> warBAB
      ICFort -> warFort
      ICWill -> warWill
      ICRefl -> warRefl
      ICSpecial -> warSpecial

-- BAB increase = f(new level)
warBAB :: Integer -> Integer
warBAB _ = 1 -- Increase by 1 every level

-- Fortitude increase = f(new level)
warFort :: Integer -> Integer
warFort 1 = 2
warFort i =
  if i `mod` 2 == 0
    then 1
    else 0

-- Will increase = f(new level)
warWill :: Integer -> Integer
warWill i =
  if i `mod` 3 == 0
    then 1
    else 0

-- Reflex increase = f(new level)
warRefl :: Integer -> Integer
warRefl i =
  if i `mod` 3 == 0
    then 1
    else 0

-- Special increases (ask for new feats, etc.) = f(new level)
warSpecial :: MonadRandom m => Integer -> Archetype m -> Character -> m Character
warSpecial _ = error "special warrior increases NIY"
