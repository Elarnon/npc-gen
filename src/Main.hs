{-# LANGUAGE OverloadedStrings, GADTs, Rank2Types #-}
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe
import Control.Monad
import Control.Monad.Random
import Generic.Abilities as Abilities
import Generic.Misc
import Generic.Race as Race
import Generic.Skill as Skill
import Generic.Feat as Feat
import Generic.Class as Class
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Generic.Character
import Generic.Archetype
import Utils

applyRace :: Archetype m -> Race -> Character -> m Character
applyRace = undefined

-- Creates a level-0 character
createCharacter :: MonadRandom m =>
  Archetype m -> DMap (Wrap Maybe CKey) -> m Character
createCharacter arch base  =
  -- Pick simple values that doesn't require decorators
  DMap.foldrWithKey (pickInitial arch) (return DMap.empty) base >>= \cMap ->
  -- Don't forget the race. This may be ugly, but…
  (case DMap.lookup CRace cMap of
    Just r -> applyRace arch r
    Nothing -> return) $ mkD cMap []
    where
      -- Insert initial values with a default
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

chrLevel :: Character -> Integer
chrLevel chr =
  Map.foldr (\c i -> clsLevel c + i) 0 . fromMaybe Map.empty
  $ DMap.lookup CClasses `mapD` chr

incFeat :: MonadRandom m =>
  Feat -> Archetype m -> Character -> m Character
incFeat = error "incFeat NIY"

incAbility :: MonadRandom m =>
  Archetype m -> Character -> Ability -> m Character
incAbility = error "incAbility NIY"

chrAbilityModifier :: Ability -> Character -> Integer
chrAbilityModifier ab chr =
  maybe 0 (\x -> (x - 10) `div` 2) $ DMap.lookup (CAbility ab) `mapD` chr

-- Increase some attribute of the character
-- TODO: this function is too big and should maybe be splitted up
levelUp :: MonadRandom m =>
  Archetype m -> Character -> m (Maybe Character)
levelUp arch chr =
  let lvl = 1 + chrLevel chr in
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
      let intMod = chrAbilityModifier INT chr
          nbSkills = clsSkills incCls + intMod + bSk
          conMod = chrAbilityModifier CON chr
      in pickN arch USkill nbSkills `mapD` chr >>= \skls ->
      -- TODO: check valid, etc.
      let nchr = decorate chr Decorator
            { ident = T.concat ["Generic class[", clsName incCls, "] level up"]
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
      in liftM Just $ incClass incCls ICSpecial lvl arch chr
-- TODO: Spells

rollDice :: MonadRandom m => Integer -> m Integer
rollDice d = getRandomR (1, d)

main :: IO ()
main = return ()
  {--let chr = createCharacter [(dumbArchetype, 1)] emptyChr 10 in
  let idChr = runIdentity $ Rand.evalRandT chr (Rand.mkStdGen 10) in
  putStrLn $ show idChr --}
