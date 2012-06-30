{-# LANGUAGE OverloadedStrings #-}
{-- data Feat
  = Athletic
  | AugmentSummoning
  deriving (Data, Typeable)

featApply Athletic =
  mapM_ (skillBonus (\v -> if v >= 10 then 4 else 2)) [Swim, Climb]
featApply AugmentSummoning =
  spellModify
    (\sp ->
      convertSpell >>= (\sp ->
      case fromDynamic sp :: SummoningSpell  of
        Nothing -> return sp
        Just summon ->
          increaseAbilities [STR, CON] (summonedCreature summon))) --}

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Control.Monad.Random as Rand
import qualified Data.Set as Set
import Data.Functor.Identity
import Data.Maybe
import Control.Monad

type Map = Map.Map
type Set = Set.Set
type Rand = Rand.Rand
type Text = T.Text

data Sex = Male | Female
  deriving (Eq, Show)

data Size = ES | S | M | B | EB
  deriving (Eq, Show)

data Race = Race Text
  deriving (Eq, Show)

data Abilities m = Abilities
  { str :: m Integer
  , dex :: m Integer
  , con :: m Integer
  , int :: m Integer
  , wis :: m Integer
  , cha :: m Integer
  }

data Ability = STR | DEX | CON | INT | WIS | CHA
  deriving (Eq, Show)

data Skill = Skill
  { skName :: Text
  , requireTraining :: Bool
  , ability :: Ability
  }
  deriving (Eq, Show)

data SkillInstance = SkillInstance
  { skill :: Skill
  , ranks :: Integer
  , bonus :: Integer
  }
  deriving (Eq, Show)

data Class = Class
  { clsName :: Text
  , clsSkills :: Integer
  , life :: Integer
  , getHpDice :: Integer
  , getNbSkills :: Integer
  }
  deriving (Eq, Show)

data ClassInstance = ClassInstance
  { cls :: Class
  , lvl :: Integer
  }
  deriving (Eq, Show)

data Alignment
  = LB
  | LM
  | LN
  | NB
  | N
  | NM
  | CB
  | CN
  | CM

newtype Feat = Feat Text
  deriving (Eq, Show)

newtype Language = Language Text
  deriving (Eq, Show)

data Character m = Character
  { name :: m Text
  , race :: m Race
  , size :: m Size
  , sex :: m Sex
  , alignment :: m Alignment
  , favouredClass :: m Class
  , hp :: m Integer
  , classes :: m (Map Text ClassInstance)
  , lvlAdjust :: m Integer
  , abilities :: m (Abilities Identity)
  , skills :: m (Map Text SkillInstance)
  , feats :: m (Map Text Feat)
  , lang :: m (Set Language)
  }

data FavBonus = BonusHp | BonusSkill

data Archetype g = Archetype
  { pickName :: Rand g Text
  , pickRace :: Rand g Race
  , pickSex :: Race -> Class -> Alignment -> Rand g Sex
  , pickAlignment :: Race -> Class -> Rand g Alignment
  , pickFavCls :: Race -> Rand g Class
  , pickStartingAbilities :: Race -> Sex -> Class -> Abilities Identity
  , pickNewClass :: Character Identity -> Identity (Rand g Class)
  , pickNewSkill :: Character Identity -> Identity (Rand g Skill)
  , pickNewFeat :: Character Identity -> Rand g Feat
  , pickNewLang :: Character Identity -> Rand g Language
  , pickNewAbility :: Character Identity -> Rand g Ability
  , pickFavBonus :: Character Identity -> Identity (Rand g FavBonus)
  }

-- TODO move to top

defaultName :: Text
defaultName = "An unknown hero"

defaultHp :: Integer
defaultHp = 0

defaultClasses :: Map Text ClassInstance
defaultClasses = Map.empty

defaultLvlAdjust :: Integer
defaultLvlAdjust = 0

defaultAbilities :: Abilities Identity
defaultAbilities = Abilities
  { str = return 10
  , dex = return 10
  , int = return 10
  , con = return 10
  , wis = return 10
  , cha = return 10
  }

defaultSkills :: Map Text SkillInstance
defaultSkills = Map.empty

defaultFeats :: Map Text Feat
defaultFeats = Map.empty

defaultLang :: Set Language
defaultLang = Set.empty

raceSize :: Race -> Size
raceSize = error "raceSize: not implemented yet"

raceAdjust :: [(Archetype g, Rational)] -> Race -> Character m -> Character m
raceAdjust = error "raceAdjust: not implemented yet"

getCharacterLevel :: Character m -> m Integer
getCharacterLevel = error "getCharacterLevel: not implemented yet"

createCharacter archetypes base lvl =
  -- Pick random "level-0" values
  pwd pickRace race                                 >>= \chrRace   ->
  pwd (\x -> pickFavCls x chrRace) favouredClass          >>= \chrFavCls ->
  pwd (\x -> pickAlignment x chrRace chrFavCls) alignment >>= \chrAlign  ->
  pwd (\x -> pickSex x chrRace chrFavCls chrAlign) sex    >>= \chrSex    ->
  -- Compute other "level-0" values
  let chrName      = fromMaybe defaultName        $ name base
      chrSize      = fromMaybe (raceSize chrRace) $ size base
      chrHp        = fromMaybe defaultHp          $ hp base
      chrClasses   = fromMaybe defaultClasses     $ classes base
      chrLvlAdjust = fromMaybe defaultLvlAdjust   $ lvlAdjust base
      chrAbilities = fromMaybe defaultAbilities   $ abilities base
      chrSkills    = fromMaybe defaultSkills      $ skills base
      chrFeats     = fromMaybe defaultFeats       $ feats base
      chrLang      = fromMaybe defaultLang        $ lang base
      -- "Level 0" character
      chr          = raceAdjust archetypes chrRace $ Character
        { name          = Identity chrName
        , alignment     = Identity chrAlign
        , race          = Identity chrRace
        , size          = Identity chrSize
        , sex           = Identity chrSex
        , favouredClass = Identity chrFavCls
        , hp            = Identity chrHp
        , classes       = Identity chrClasses
        , lvlAdjust     = Identity chrLvlAdjust
        , abilities     = Identity chrAbilities
        , skills        = Identity chrSkills
        , feats         = Identity chrFeats
        , lang          = Identity chrLang
        } in
  composeN lvl (>>= addLevel archetypes) . return $ chr
  where
    -- pwd == pickWithDefault
    pwd pick val = 
      maybe
        (Rand.fromList archetypes >>= \archetype ->
         -- tell ("Picked archetype " ++ show archetype) >>
         pick archetype)
        ({--(tell "Default value." >>) . --}return)
        (val base)

composeN :: Integral a => a -> (t -> t) -> t -> t
composeN 0 f = id
composeN n f = f . composeN (n-1) f

-- Number of times to try when archetypes return bad values
nbTry :: Integer
nbTry = 5

addFeat :: [(Archetype g, Rational)] -> Character m -> Rand g (Character m)
addFeat archetypes chr = error "addFeat: not implemented yet" {--
  pick pickFeat archetypes chr >>= \feat ->
  if hasFeat feat chr && not isCumulable feat
    -- TODO: recursion limitation
    then addFeat archetypes chr
    else chr { feats =  --}

-- feats :: Set Feat ? Map Feat FeatInstance ?

addAbility :: [(Archetype g, Rational)] -> Character m -> Rand g (Character m)
addAbility = error "addAbility: not implemented yet"

pick :: (Archetype g -> Character m -> m (Rand g a)) -> [(Archetype g, Rational)] -> Character m -> Rand g (m a)
pick = error "pick: not implemented yet"

convertFavBonus :: FavBonus -> (Integer, Integer)
convertFavBonus = error "convertFavBonus: not implemented yet"

rollDice :: Integer -> Rand g Integer
rollDice = error "rollDice: not implemented yet"

abilityModifier :: Ability -> Character m -> Integer
abilityModifier = error "abilityModifier: not implemented yet"

classLvlUp :: Class -> [(Archetype g, Rational)] -> Character m -> Rand g (Character m)
classLvlUp = error "classLvlup: not implemented yet"

addLvls ci1 ci2 = ci2 { lvl = lvl ci1 + lvl ci2 }

addLevel archetypes chr = -- {{{
  let lvl = runIdentity $ (getCharacterLevel chr :: Identity Integer) in
  -- Add feats when needed
  (if lvl `mod` 2 == 0 || lvl == 0
      then addFeat archetypes chr -- pick pickFeat archetypes chr
      else return chr) >>= \chr ->
  -- Add abilities when needed
  (if lvl `mod` 4 == 3 || lvl == 0
      then addAbility archetypes chr -- pick pickAbility ...
      else return chr) >>= \chr ->
  -- Choose a class to level up
  pick pickNewClass archetypes chr >>= \incCls ->
  -- Apply eventual favoured class bonuses
  (if runIdentity incCls == (runIdentity $ favouredClass chr)
    then liftM convertFavBonus $ liftM runIdentity $ pick (pickFavBonus {--incCls--}) archetypes chr
    else return (0, 0)) >>= \(bonusSkill, bonusHp) ->
  rollDice (getHpDice . runIdentity $ incCls) >>= \clsHpInc ->
  -- Update hp
  let cls' = Map.insertWith' addLvls (clsName $ runIdentity incCls) (ClassInstance { cls = undefined, lvl = 1 }) $ runIdentity $ classes chr
      nbSkills = (getNbSkills $ runIdentity incCls) + (abilityModifier INT chr) + bonusSkill
      addHp = clsHpInc + (abilityModifier CON chr) + bonusHp
      chr' = chr { classes = Identity cls' , hp = Identity $ (runIdentity $ hp chr) + addHp } in
  -- Update skills
  (composeN nbSkills (>>= addSkill nbTry archetypes) . return ) chr' >>= \chr ->
  -- Class-specific stuff
  classLvlUp (runIdentity incCls) archetypes chr -- }}}

getSkillLvl :: Skill -> Character m -> m Integer
getSkillLvl = error "getSkillLvl: not implemented yet"

addRank :: SkillInstance -> SkillInstance -> SkillInstance
addRank = error "addRank: not implemented yet"

addSkill 0 archetypes chr = return chr
addSkill try archetypes chr =
  let lvl = runIdentity $ getCharacterLevel chr in
  pick pickNewSkill archetypes chr >>= \skill ->
  if runIdentity (getSkillLvl (runIdentity skill) chr) >= lvl
    then addSkill (try - 1) archetypes chr
    else return $ chr { skills = Identity $ Map.insertWith' addRank (skName $ runIdentity skill) (SkillInstance { skill = undefined, ranks = 1, bonus = undefined }) $ runIdentity $ skills chr }

main :: IO ()
main = return ()
