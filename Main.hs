{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Control.Monad.Random as Rand
import qualified Data.Set as Set
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Pathfinder.Abilities as Abilities
import Pathfinder.Misc
import Pathfinder.Race as Race
import Pathfinder.Skill as Skill
import Pathfinder.Feat as Feat
import Pathfinder.Class as Class
import Pathfinder.Character

type Map = Map.Map
type Set = Set.Set
type Rand = Rand.Rand
type RandT = Rand.RandT
type Text = T.Text

data FavBonus = BonusHp | BonusSkill

data Archetype g m = Archetype
  { pickName :: RandT g m Text
  , pickRace :: RandT g m Race
  , pickSex :: Race -> Class -> Alignment -> RandT g m Sex
  , pickAlignment :: Race -> Class -> RandT g m Alignment
  , pickFavCls :: Race -> RandT g m Class
  , pickStartingAbilities :: Race -> Sex -> Class -> Rand g (Abilities m)
  , pickNewClass :: Character m -> RandT g m Class
  , pickNewSkill :: Character m -> RandT g m Skill
  , pickNewFeat :: Character m -> RandT g m Feat
  , pickNewLang :: Character m -> RandT g m Language
  , pickNewAbility :: Character m -> RandT g m Ability
  , pickFavBonus :: Character m -> RandT g m FavBonus
  }
{--
dumbArchetype :: Rand.RandomGen g => Archetype g Identity
dumbArchetype = Archetype
  { pickName = return "First name."
  , pickRace = return Human
  , pickSex = \_ _ _ -> return Male
  , pickAlignment = \_ _ -> return N
  , pickFavCls = \_ -> return $ Class { clsName = "Guerrier", clsSkills = 2, life = 10, getHpDice = 10 }
  , pickStartingAbilities = \_ _ _ ->
                              return $
                                updateAbility (+8) STR $
                                updateAbility ((-)2) INT $
                                updateAbility (+4) CON $
                                Abilities.defaultAbilities
  , pickNewClass = \_ -> return $ Class { clsName = "Guerrier", clsSkills = 2, life = 10, getHpDice = 10 }
  , pickNewSkill = \_ -> return $ Skill { skName = "Taper", requireTraining = True, ability = STR }
  , pickNewFeat = \_ -> return $ Feat "Combat à mains nues"
  , pickNewLang = \_ -> return $ Language "Eguzien"
  , pickNewAbility = \_ -> return $ STR
  , pickFavBonus = \_ -> return $ BonusHp
  }
  --}

createCharacter :: (Monad m, Rand.RandomGen g) =>
  [(Archetype g m, Rational)]
    -> Character Maybe -> Integer -> RandT g m (Character m)
createCharacter archetypes base lvl =
  -- Pick random "level-0" values
  pwd pickRace race                                 >>= \chrRace   ->
  pwd (\x -> pickFavCls x chrRace) favouredClass          >>= \chrFavCls ->
  pwd (\x -> pickAlignment x chrRace chrFavCls) alignment >>= \chrAlign  ->
  pwd (\x -> pickSex x chrRace chrFavCls chrAlign) sex    >>= \chrSex    ->
  -- Compute other "level-0" values
  let chrName      = fromMaybe defaultName           $ name base
      chrSize      = fromMaybe (raceSize chrRace)    $ size base
      chrHp        = fromMaybe defaultHp             $ hp base
      chrClasses   = fromMaybe defaultClasses        $ classes base
      chrLvlAdjust = fromMaybe defaultLvlAdjust      $ lvlAdjust base
      chrAbilities = Abilities.fromDefault defaultAbilities $ abilities base
      chrSkills    = fromMaybe Skill.defaultSkills   $ skills base
      chrFeats     = fromMaybe defaultFeats          $ feats base
      chrLang      = fromMaybe defaultLanguages      $ lang base
      -- "Level 0" character
      chr          = raceAdjust chrRace $ Character
        { name          = return chrName
        , alignment     = return chrAlign
        , race          = return chrRace
        , size          = return chrSize
        , sex           = return chrSex
        , favouredClass = return chrFavCls
        , hp            = return chrHp
        , classes       = return chrClasses
        , lvlAdjust     = return chrLvlAdjust
        , abilities     = chrAbilities
        , skills        = return chrSkills
        , feats         = return chrFeats
        , lang          = return chrLang
        } in
  composeN lvl (>>= addLevel archetypes) $ chr
  where
    -- pwd == pickWithDefault
    pwd pick val = 
      maybe
        (Rand.fromList archetypes >>= \archetype ->
         -- tell ("Picked archetype " ++ show archetype) >>
         pick archetype)
        ({--(tell "Default value." >>) . --}return)
        (val base) --}

composeN :: Integral a => a -> (t -> t) -> t -> t
composeN 0 f = id
composeN n f = f . composeN (n-1) f

-- Number of times to try when archetypes return bad values
nbTry :: Integer
nbTry = 5

hasFeat :: Monad m => Feat -> Character m -> m Bool
hasFeat ft chr =
  feats chr >>= \fts ->
  return $ Set.member ft fts

addFeat :: (Monad m, Rand.RandomGen g) =>
  Integer -> [(Archetype g m, Rational)] -> Character m -> RandT g m (Character m)
addFeat 0 _ chr = return chr
addFeat try archetypes chr = 
  pick pickNewFeat archetypes chr >>= \feat -> 
  lift (hasFeat feat chr) >>= \hasFeat' ->
  if hasFeat' -- now included in hasFeat && not (isCumulable feat)
    then addFeat (try - 1) archetypes chr
    else 
      lift (feats chr) >>= \feats' ->
      return $ chr { feats = return $ Set.insert feat feats' }

addAbility :: (Monad m, Rand.RandomGen g) =>
  [(Archetype g m, Rational)] -> Character m -> RandT g m (Character m)
addAbility archetypes chr =
  let abs = abilities chr in
  pick pickNewAbility archetypes chr >>= \ab ->
  return $ chr {
    abilities = updateAbility (+1) ab abs
  }

pick :: (Monad m, Rand.RandomGen g) =>
  (Archetype g m -> Character m -> RandT g m a) -> [(Archetype g m, Rational)]
    -> Character m -> RandT g m a
pick picker archetypes chr =
  Rand.fromList archetypes >>= \arch ->
  picker arch chr

convertFavBonus :: FavBonus -> (Integer, Integer)
convertFavBonus BonusHp = (0, 1)
convertFavBonus BonusSkill = (1, 0)

rollDice :: (Monad m, Rand.RandomGen g) => Integer -> RandT g m Integer
rollDice d = Rand.getRandomR (1, d)

abilityModifier :: Monad m => Ability -> Character m -> m Integer
abilityModifier ab chr = 
  getAbility ab (abilities chr) >>= \val ->
  return $ (val - 10) `div` 2

classLvlUp :: Monad m => 
  Class -> [(Archetype g m, Rational)] -> Character m
    -> RandT g m (Character m)
classLvlUp _cls _atypes chr = return chr -- TODO: error "classLvlup: not implemented yet"

addLvls ci1 ci2 = ci2 { lvl = lvl ci1 + lvl ci2 }

addLevel :: (Monad m, Rand.RandomGen g) =>
  [(Archetype g m, Rational)] -> Character m -> RandT g m (Character m)
addLevel archetypes chr = -- {{{
  lift (getCharacterLevel chr) >>= \lvl ->
  -- Add feats when needed
  (if lvl `mod` 2 == 0 || lvl == 0
      then addFeat nbTry archetypes chr -- pick pickFeat archetypes chr
      else return chr) >>= \chr ->
  -- Add abilities when needed
  (if lvl `mod` 4 == 3 || lvl == 0
      then addAbility archetypes chr -- pick pickAbility ...
      else return chr) >>= \chr ->
  -- Choose a class to level up
  pick pickNewClass archetypes chr >>= \incCls ->
  -- Apply eventual favoured class bonuses
  lift (favouredClass chr) >>= \favCls ->
  (if incCls == favCls
    then liftM convertFavBonus $ pick (pickFavBonus {--incCls--}) archetypes chr
    else return (0, 0)) >>= \(bonusSkill, bonusHp) ->
  rollDice (getHpDice incCls) >>= \clsHpInc ->
  lift (classes chr) >>= \cls ->
  lift (abilityModifier INT chr) >>= \intMod ->
  lift (abilityModifier CON chr) >>= \conMod ->
  -- Update hp
  let cls' = Map.insertWith'
              addLvls
              incCls
              (Class.newInstance 1)
              cls
      nbSkills = (clsSkills incCls) + intMod + bonusSkill
      addHp = clsHpInc + conMod + bonusHp
      chr' = chr { classes = return cls' , hp = liftM (+ addHp) (hp chr) } in
  -- Update skills
  (composeN nbSkills (>>= addSkill nbTry archetypes) . return) chr' >>= \chr ->
  -- Class-specific stuff
  classLvlUp incCls archetypes chr

getSkillRanks :: Monad m => Skill -> Character m -> m Integer
getSkillRanks sk chr =
  skills chr >>= \skls ->
  return . fromMaybe 0 . liftM ranks $ Map.lookup sk skls

updateSkillBonus :: Character m -> Character m
updateSkillBonus = \x -> x-- TODO: error "updateSkillBonus: not implemented yes"

addSkill :: (Monad m, Rand.RandomGen g) =>
  Integer -> [(Archetype g m, Rational)] -> Character m
    -> RandT g m (Character m)
addSkill 0 archetypes chr = return chr
addSkill try archetypes chr =
  lift (getCharacterLevel chr) >>= \lvl ->
  pick pickNewSkill archetypes chr >>= \sk ->
  lift (getSkillRanks sk chr) >>= \skLvl ->
  if skLvl >= lvl
    then addSkill (try - 1) archetypes chr
    else 
      lift (skills chr) >>= \skls ->
      let newInstance = Skill.makeInstance 1
          skls' = Map.insertWith' Skill.addRank sk newInstance skls
      in
      return $ updateSkillBonus $ chr { skills = return skls' }

emptyChr = Character
  { name = Nothing, race = Nothing, size = Nothing, sex = Nothing, alignment = Nothing,
           favouredClass = Nothing, hp = Nothing, classes = Nothing, lvlAdjust = Nothing,
            abilities = defaultAbilities, skills = Nothing, feats = Nothing,
                        lang = Nothing }

main :: IO ()
main = return ()
  {--let chr = createCharacter [(dumbArchetype, 1)] emptyChr 10 in
  let idChr = runIdentity $ Rand.evalRandT chr (Rand.mkStdGen 10) in
  putStrLn $ show idChr --}
