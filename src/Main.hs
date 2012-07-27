{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import Pathfinder.Abilities as Abilities
import Pathfinder.Misc
import Pathfinder.Race as Race
import Pathfinder.Skill as Skill
import Pathfinder.Feat as Feat
import Pathfinder.Class as Class
import Pathfinder.Character
import Utils

type Map = Map.Map
type Set = Set.Set
type Text = T.Text

data Archetype m = Archetype
  { pickBaseChar :: m Character -- WOOOOHOO !!!
  , pickRace :: m Race
  , pickNewClass :: Character -> m Class
  , pickNewSkill :: Character -> [Skill] -> m Skill
  , pickNewFeat :: Character -> m Feat
  , pickNewLang :: Character -> m Language
  , pickNewAbility :: Character -> m Ability
  , pickFavBonus :: Character -> m FavBonus
  }

{-
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
  -}

-- theRealCharacter == MonadRandom m => Decorated Character m

{-
createCharacter :: MonadRandom m =>
  [(Archetype m, Rational)] -> Integer -> m (Decorated Character)
createCharacter archetypes lvl =
  -- Pick random "level-0" character
  pick pickRace archetypes >>= \race ->
  pick pickFavCls archetypes >>= \favCls ->
  pick pickAlignment archetypes >>= \align ->
  pick pickSex archetypes >>= \sex ->
  race (Character { blablabla }) >>= \chr -> undefined
  -}

{-
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
        (val base) 

-- Number of times to try when archetypes return bad values
nbTry :: Integer
nbTry = 5
-}

type CharacterD = Decorated Character

hasFeat :: Feat -> Character -> Bool
hasFeat ft = Set.member ft . chrFeats

addFeat :: MonadRandom m =>
  [(Archetype m, Rational)] -> CharacterD -> m CharacterD
addFeat archetypes chr =
  pick pickNewFeat archetypes (runD chr) >>= \feat ->
  if hasFeat feat `mapD` chr
    then return chr -- TODO: pick another one
    else return . decorate chr $ Decorator
          { ident = T.pack $ "Add feat " ++ show feat
          , fun = \chr -> chr { chrFeats = Set.insert feat $ chrFeats chr }
          }

increaseAbility :: MonadRandom m =>
  [(Archetype m, Rational)] -> CharacterD -> m CharacterD
increaseAbility archetypes chr =
  pick pickNewAbility archetypes (runD chr) >>= \ab ->
  return . decorate chr $ Decorator
    { ident = undefined -- TODO "Increase ability " ++ show ab
    , fun = chrUpdateAbility (+1) ab
    }

pick :: MonadRandom m =>
  (a -> b -> m c) -> [(a, Rational)]
    -> b -> m c
pick picker args1 arg2 = fromList args1 >>= flip picker arg2

-- TODO: rewrite that shit so that it is readable
pickN :: MonadRandom m =>
  (a -> b -> [c] -> m c) -> Integer -> [(a, Rational)] -> b -> m [c]
pickN picker nb args1 arg2 = pickN' picker nb args1 arg2 []
  where
    pickN' picker 0 args1 arg2 arg3 = return arg3
    pickN' picker n args1 arg2 arg3 =
      pick (\a1 a2 -> picker a1 a2 arg3) args1 arg2 >>= \a3 ->
      pickN' picker (n-1) args1 arg2 (a3:arg3)

convertFavBonus :: FavBonus -> (Integer, Integer)
convertFavBonus BonusHp = (0, 1)
convertFavBonus BonusSkill = (1, 0)

rollDice :: MonadRandom m => Integer -> m Integer
rollDice d = getRandomR (1, d)

chrClsLvlUp :: MonadRandom m =>
  Class -> [(Archetype m, Rational)] -> CharacterD -> m CharacterD
chrClsLvlUp = error "chrClsLvlUp :: not implemented"

addLevel :: MonadRandom m =>
  [(Archetype m, Rational)] -> CharacterD -> m CharacterD
addLevel archetypes chr =
  let lvl = chrLevel $ runD chr in
  -- Add feats when needed
  (if lvl `mod` 2 == 0 || lvl == 0
    then addFeat archetypes chr
    else return chr) >>= \chr ->
  -- Add abilities when needed
  (if lvl `mod` 4 == 3 || lvl == 0
    then increaseAbility archetypes chr
    else return chr) >>= \chr ->
  -- Choose a class to level up TODO: check valid to level up
  pick pickNewClass archetypes (runD chr) >>= \incCls ->
  -- Get favoured class bonuses
  (if incCls == chrFavCls `mapD` chr
    then liftM convertFavBonus $ pick pickFavBonus archetypes $ runD chr
    else return (0, 0)) >>= \(bSk, bHp) ->
  -- Roll HP dice
  rollDice (clsHpDice incCls) >>= \hpInc ->
  let intMod = chrAbilityModifier INT `mapD` chr
      nbSkills = clsSkills incCls + intMod + bSk
      conMod = chrAbilityModifier CON `mapD` chr
  in pickN pickNewSkill nbSkills archetypes `mapD` chr >>= \skls ->
  chrClsLvlUp incCls archetypes $
      decorate chr $ Decorator
        { ident = "Level up class"
        , fun = \chr -> 
                chr { chrClasses = 
                        Map.insertWith'
                          clsAddLevels
                          incCls
                          (ClassInstance 1)
                          $ chrClasses chr
                    , hp = hp chr + hpInc + conMod + bHp
                    , chrSkills =
                      foldr
                        (\sk -> Map.insertWith' skAdd sk $ SkillInstance 1 0)
                        (chrSkills chr)
                        skls
                    }
        }


{-
getSkillRanks :: Monad m => Skill -> Character -> m Integer
getSkillRanks sk chr =
  skills chr >>= \skls ->
  return . fromMaybe 0 . liftM ranks $ Map.lookup sk skls

updateSkillBonus :: Character m -> Character m
updateSkillBonus = \x -> x-- TODO: error "updateSkillBonus: not implemented yes"
-}
{-
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
-}

main :: IO ()
main = return ()
  {--let chr = createCharacter [(dumbArchetype, 1)] emptyChr 10 in
  let idChr = runIdentity $ Rand.evalRandT chr (Rand.mkStdGen 10) in
  putStrLn $ show idChr --}
