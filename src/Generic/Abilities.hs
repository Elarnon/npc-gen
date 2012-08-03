module Generic.Abilities
  ( Abilities
  , Ability (..)
  , abSetAbility
  , abGetAbility
  , abUpdateAbility
  , abModifier
  ) where

data Abilities = Abilities
  { str :: Integer
  , dex :: Integer
  , con :: Integer
  , int :: Integer
  , wis :: Integer
  , cha :: Integer
  }
  deriving (Eq, Show)

data Ability = STR | DEX | CON | INT | WIS | CHA
  deriving (Eq, Show, Ord)

abSetAbility :: Ability -> Integer -> Abilities -> Abilities
abSetAbility ab i as =
  case ab of
    STR -> as { str = i }
    DEX -> as { dex = i }
    CON -> as { con = i }
    INT -> as { int = i }
    WIS -> as { wis = i }
    CHA -> as { cha = i }

abGetAbility :: Ability -> Abilities -> Integer
abGetAbility ab =
  case ab of
    STR -> str
    DEX -> dex
    CON -> con
    INT -> int
    WIS -> wis
    CHA -> cha

abUpdateAbility :: (Integer -> Integer) -> Ability -> Abilities -> Abilities
abUpdateAbility f ab abs =
  abSetAbility ab (f $ abGetAbility ab abs) abs

abModifier :: Ability -> Abilities -> Integer
abModifier ab abs = (abGetAbility ab abs - 10) `div` 2
