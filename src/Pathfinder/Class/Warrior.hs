module Pathfinder.Class.Warrior where

import Pathfinder.Class
import Pathfinder.Archetype
import Pathfinder.NewCharacter

data Warrior = Warrior

instance Class Warrior where
  clsName Warrior = "Warrior"
  clsLife Warrior = 10
  clsHpDice Warrior = 10
  clsSkills Warrior = 10
  incClass Warrior c =
    case c of
      ICBAB -> warBAB
      ICFort -> warFort
      ICWill -> warWill
      ICRefl -> warRefl
      ICSpecial -> warSpecial

warBAB :: Integer -> Integer
warBAB _ = 1 -- Increase by 1 every level

warFort :: Integer -> Integer
warFort 1 = 2
warFort i =
  if i `mod` 2 == 0
    then 1
    else 0

warWill :: Integer -> Integer
warWill i =
  if i `mod` 3 == 0
    then 1
    else 0

warRefl :: Integer -> Integer
warRefl i =
  if i `mod` 3 == 0
    then 1
    else 0

warSpecial :: Integer -> 
warSpecial _ = undefined
