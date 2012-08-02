{-# LANGUAGE OverloadedStrings, GADTs, Rank2Types #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
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
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Pathfinder.NewCharacter
import Pathfinder.Archetype
import Utils

{-
data family CKey k :: *

data instance CKey Race = CRace

data CUpdateKey m a where
  CULife :: CUpdateKey m (Character -> m Integer)

data CKey a where
  CUpdateKey :: CUpdateKey m (Character -> m a) -> CKey a
  CRace :: CKey Race

data WrapTyCon t tag a = Wrapped t (tag a)

instance (Eq t, GEq tag) => GEq (WrapTyCon t tag) where
  geq (Wrapped ty tag) (Wrapped ty' tag') =
    if ty == ty' then geq tag tag' else Nothing

instance (Ord t, GCompare tag) => GCompare (WrapTyCon t tag) where
  gcompare (Wrapped ty tag) (Wrapped ty' tag') =
    case compare ty ty' of
      EQ -> gcompare tag tag'
      LT -> GLT
      GT -> GGT

data KeyWithD a where
  KeyWithD :: CKey a -> [CDecorator a] -> KeyWithF a

data Character = Character { getChr :: Decorated (DMap CKey) }
-}
-- To construct a character I need
-- 1) default values i.e. CKey a -> a
-- 2) archetypes
{-
-- Now, archetypes are what ?
-- You must say things like
-- « ok boy I want to do *that* »
-- so basically
-- they are Character -> CKey a ?
-- nah.
-- I should also have a DKey
-- and archetypes would be DKey -> Maybe (Character -> Character)
-- Ok now i have one last problem
-- that is hrrm
-- what
-- oh yeah
-- i have to distinguish default values /= updates
-- so
-- basically, i have a "CMap CKey" for default values
-- and a "CMap (b -> CKey {{ Character -> m b }})
-}


randFromList :: MonadRandom m => [(a, Rational)] -> m (a, [(a, Rational)])
randFromList = undefined

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
-- archPickValue :: Archetype m -> CKey a -> DMap CKey -> m (Maybe a)

get :: WithDefault a => CKey a -> DMap CKey -> a
get k dmap = maybeDefault $ DMap.lookup k dmap

class WithDefault a where
  defaultValue :: a

instance WithDefault (Map k a) where
  defaultValue = Map.empty

maybeDefault :: WithDefault a => Maybe a -> a
maybeDefault (Just x) = x
maybeDefault Nothing = defaultValue

pickDefault :: (MonadRandom m, WithDefault a, ArchTag t) =>
  Archetype m -> t a -> DMap CKey -> m a
pickDefault arch k dmap = liftM maybeDefault $ archPickValue arch k dmap

-- Creates a level-0 character
createCharacter :: MonadRandom m =>
  Archetype m -> DMap (Wrap Maybe CKey) -> m Character
createCharacter arch base =
  DMap.foldrWithKey (pickInitial arch) (return DMap.empty) base >>= \chrMap ->
  return $ mkD chrMap [] -- TODO : race & stuff
    where
      pickInitial :: MonadRandom m => 
        Archetype m -> Wrap Maybe CKey v -> v -> m (DMap CKey) -> m (DMap CKey)
      pickInitial a (Wrapped k) v mChr =
        mChr >>= \chr ->
        case v of
          Just x  -> return $ DMap.insert k x chr
          Nothing ->
            archPickValue a k chr >>= \val ->
            case val of
              Just x -> return $ DMap.insert k x chr
              Nothing -> return chr

chrLevel :: DMap CKey -> Integer
chrLevel dmap = Map.foldr (\c i -> clsLevel c + i) 0 $ get CClasses dmap

incFeat :: MonadRandom m =>
  Feat -> Archetype m -> Character -> m Character
incFeat = undefined

incAbility :: MonadRandom m =>
  Archetype m -> Character -> Ability -> m Character
incAbility = undefined

chrAbilityModifier :: Ability -> DMap CKey -> Integer
chrAbilityModifier ab dmap =
  case DMap.lookup (CAbility ab) dmap of
    Nothing -> 0
    Just x -> (x - 10) `div` 2

-- Increase some attribute of the character
levelUp :: MonadRandom m =>
  Archetype m -> Character -> m (Maybe Character)
levelUp arch chr =
  let lvl = 1 + chrLevel `mapD` chr in
  -- Add feats when needed
  (if lvl `mod` 2 == 1 || lvl == 1
    then archPickValue arch UFeat `mapD` chr >>= \v ->
          case v of
            Just x -> incFeat x arch chr
            Nothing -> return chr
    else return chr) >>= \chr ->
  -- Add abilities when needed
  (if lvl `mod` 4 == 0 || lvl == 1
    then archPickValue arch UAbility `mapD` chr >>= \v ->
          case v of
            Just x -> incAbility arch chr x
            Nothing -> return chr
    else return chr) >>= \chr ->
  -- Choose a class to level up TODO: check valid to level up
  archPickValue arch UClass `mapD` chr >>= \mIncCls ->
  case mIncCls of
    Nothing -> return Nothing
    Just incCls ->
      -- Get favoured class bonuses
      (if Just incCls == DMap.lookup CFavCls `mapD` chr
        then liftM (maybe (0, 0) convertFavBonus) $ archPickValue arch (UFavBonus incCls) `mapD` chr
        else return (0, 0)) >>= \(bSk, bHp) ->
      -- Roll HP dice
      rollDice (clsHpDice incCls) >>= \hpInc ->
      let intMod = chrAbilityModifier INT `mapD` chr
          nbSkills = clsSkills incCls + intMod + bSk
          conMod = chrAbilityModifier CON `mapD` chr
      in pickN arch USkill nbSkills `mapD` chr >>= \skls ->
      -- TODO: check valid, etc.
      let nchr = decorate chr Decorator
            { ident = "Level up class"
            , fun = DMap.insertWith'
                      (Map.unionWith clsAddLevels)
                      CClasses
                      (Map.singleton incCls (ClassInstance 1))
                    . DMap.insertWith'
                      (+)
                      CLife
                      (hpInc + conMod + bHp)
                    . DMap.insertWith'
                      (Map.unionWith skAdd)
                      CSkills
                      (Map.fromList $ map (\sk -> (sk, SkillInstance 1)) skls)
                    . DMap.insertWith' (+) CBAB  (incClass incCls icBAB  lvl)
                    . DMap.insertWith' (+) CFort (incClass incCls icFort lvl)
                    . DMap.insertWith' (+) CWill (incClass incCls icWill lvl)
                    . DMap.insertWith' (+) CRefl (incClass incCls icRefl lvl)
            }
      in incClassSpecials incCls lvl arch chr
-- TODO: Spells

incClassSpecials :: (MonadRandom m, Class c) =>
  c -> Integer -> Archetype m -> Character -> m (Maybe Character)
incClassSpecials = undefined

convertFavBonus :: FavBonus -> (Integer, Integer)
convertFavBonus BonusHp = (0, 1)
convertFavBonus BonusSkill = (1, 0)

rollDice :: MonadRandom m => Integer -> m Integer
rollDice d = getRandomR (1, d)
{-
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


-- TODO: rewrite that shit so that it is readable
pickN :: MonadRandom m =>
  (a -> b -> [c] -> m c) -> Integer -> [(a, Rational)] -> b -> m [c]
pickN picker nb args1 arg2 = pickN' picker nb args1 arg2 []
  where
    pickN' picker 0 args1 arg2 arg3 = return arg3
    pickN' picker n args1 arg2 arg3 =
      pick (\a1 a2 -> picker a1 a2 arg3) args1 arg2 >>= \a3 ->
      pickN' picker (n-1) args1 arg2 (a3:arg3)


chrClsLvlUp :: MonadRandom m =>
  Class -> [(Archetype m, Rational)] -> CharacterD -> m CharacterD
chrClsLvlUp = error "chrClsLvlUp :: not implemented"


{-
getSkillRanks :: Monad m => Skill -> Character -> m Integer
getSkillRanks sk chr =
  skills chr >>= \skls ->
  return . fromMaybe 0 . liftM ranks $ Map.lookup sk skls

updateSkillBonus :: Character m -> Character m
updateSkillBonus = \x -> x-- TODO: error "updateSkillBonus: not implemented yes"
-}
-}
main :: IO ()
main = return ()
  {--let chr = createCharacter [(dumbArchetype, 1)] emptyChr 10 in
  let idChr = runIdentity $ Rand.evalRandT chr (Rand.mkStdGen 10) in
  putStrLn $ show idChr --}
