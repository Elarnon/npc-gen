{-# LANGUAGE OverloadedStrings #-}
module Pathfinder.Skill
  ( Skill (..)
  , SkillInstance (..)
  , allSkills
  , makeInstance
  , skAdd
  , utilisationD'ObjetsMagiques
  , deguisement
  , dressage
  , connaissancesReligion
  , connaissancesGeographie
  , connaissancesExplorationSouterraine
  , sabotage
  , connaissancesPlans
  , perception
  , acrobaties
  , connaissancesNoblesse
  , connaissancesMysteres
  , intimidation
  , equitation
  , escamotage
  , bluff
  , artDeLaMagie
  , artisanat
  , connaissancesFolkloreLocal
  , diplomatie
  , linguistique
  , connaissancesNature
  , evasion
  , estimation
  , representation
  , connaissancesHistoire
  , connaissancesIngenierie
  , discretion
  , profession
  , vol
  , psychologie
  , natation
  , escalade
  , survie
  , premiersSecours
  ) where

import Pathfinder.Abilities
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)

-- The datatype for a skill. Nothing to see here, really.
data Skill = Skill
  { skName :: Text
  , requireTraining :: Bool
  , ability :: Ability
  }
  deriving (Eq, Show, Ord)

-- The instance associated with a skill
-- bonus should now be computed on the fly
data SkillInstance = SkillInstance
  { ranks :: Integer
  -- , bonus :: Integer
  }
  deriving (Eq, Show)

allSkills :: Set Skill
allSkills = undefined -- TODO

makeInstance :: Integer -> SkillInstance
makeInstance i = SkillInstance { ranks = i {-, bonus = 0-} }

skAdd :: SkillInstance -> SkillInstance -> SkillInstance
skAdd sk1 sk2 =
  SkillInstance { ranks = ranks sk1 + ranks sk2{-, bonus = bonus sk1 + bonus sk2-} }

-- Auto-generated
utilisationD'ObjetsMagiques :: Skill
utilisationD'ObjetsMagiques = Skill
  { skName          = "Utilisation d'objets magiques"
  , requireTraining = True
  , ability         = CHA
  }

deguisement :: Skill
deguisement = Skill
  { skName          = "Déguisement"
  , requireTraining = False
  , ability         = CHA
  }

dressage :: Skill
dressage = Skill
  { skName          = "Dressage"
  , requireTraining = True
  , ability         = CHA
  }

connaissancesReligion :: Skill
connaissancesReligion = Skill
  { skName          = "Connaissances (religion)"
  , requireTraining = True
  , ability         = INT
  }

connaissancesGeographie :: Skill
connaissancesGeographie = Skill
  { skName          = "Connaissances (géographie)"
  , requireTraining = True
  , ability         = INT
  }

connaissancesExplorationSouterraine :: Skill
connaissancesExplorationSouterraine = Skill
  { skName          = "Connaissances (exploration souterraine)"
  , requireTraining = True
  , ability         = INT
  }

sabotage :: Skill
sabotage = Skill
  { skName          = "Sabotage"
  , requireTraining = True
  , ability         = DEX
  }

connaissancesPlans :: Skill
connaissancesPlans = Skill
  { skName          = "Connaissances (plans)"
  , requireTraining = True
  , ability         = INT
  }

perception :: Skill
perception = Skill
  { skName          = "Perception"
  , requireTraining = False
  , ability         = WIS
  }

acrobaties :: Skill
acrobaties = Skill
  { skName          = "Acrobaties"
  , requireTraining = False
  , ability         = DEX
  }

connaissancesNoblesse :: Skill
connaissancesNoblesse = Skill
  { skName          = "Connaissances (noblesse)"
  , requireTraining = True
  , ability         = INT
  }

connaissancesMysteres :: Skill
connaissancesMysteres = Skill
  { skName          = "Connaissances (mystères)"
  , requireTraining = True
  , ability         = INT
  }

intimidation :: Skill
intimidation = Skill
  { skName          = "Intimidation"
  , requireTraining = False
  , ability         = CHA
  }

equitation :: Skill
equitation = Skill
  { skName          = "Équitation"
  , requireTraining = False
  , ability         = DEX
  }

escamotage :: Skill
escamotage = Skill
  { skName          = "Escamotage"
  , requireTraining = True
  , ability         = DEX
  }

bluff :: Skill
bluff = Skill
  { skName          = "Bluff"
  , requireTraining = False
  , ability         = CHA
  }

artDeLaMagie :: Skill
artDeLaMagie = Skill
  { skName          = "Art de la magie"
  , requireTraining = True
  , ability         = INT
  }

artisanat :: Skill
artisanat = Skill
  { skName          = "Artisanat"
  , requireTraining = False
  , ability         = INT
  }

connaissancesFolkloreLocal :: Skill
connaissancesFolkloreLocal = Skill
  { skName          = "Connaissances (folklore local)"
  , requireTraining = True
  , ability         = INT
  }

diplomatie :: Skill
diplomatie = Skill
  { skName          = "Diplomatie"
  , requireTraining = False
  , ability         = CHA
  }

linguistique :: Skill
linguistique = Skill
  { skName          = "Linguistique"
  , requireTraining = True
  , ability         = INT
  }

connaissancesNature :: Skill
connaissancesNature = Skill
  { skName          = "Connaissances (nature)"
  , requireTraining = True
  , ability         = INT
  }

evasion :: Skill
evasion = Skill
  { skName          = "Évasion"
  , requireTraining = False
  , ability         = DEX
  }

estimation :: Skill
estimation = Skill
  { skName          = "Estimation"
  , requireTraining = False
  , ability         = INT
  }

representation :: Skill
representation = Skill
  { skName          = "Représentation"
  , requireTraining = False
  , ability         = CHA
  }

connaissancesHistoire :: Skill
connaissancesHistoire = Skill
  { skName          = "Connaissances (histoire)"
  , requireTraining = True
  , ability         = INT
  }

connaissancesIngenierie :: Skill
connaissancesIngenierie = Skill
  { skName          = "Connaissances (ingénierie)"
  , requireTraining = True
  , ability         = INT
  }

discretion :: Skill
discretion = Skill
  { skName          = "Discrétion"
  , requireTraining = False
  , ability         = DEX
  }

profession :: Skill
profession = Skill
  { skName          = "Profession"
  , requireTraining = True
  , ability         = WIS
  }

vol :: Skill
vol = Skill
  { skName          = "Vol"
  , requireTraining = False
  , ability         = DEX
  }

psychologie :: Skill
psychologie = Skill
  { skName          = "Psychologie"
  , requireTraining = False
  , ability         = WIS
  }

natation :: Skill
natation = Skill
  { skName          = "Natation"
  , requireTraining = False
  , ability         = STR
  }

escalade :: Skill
escalade = Skill
  { skName          = "Escalade"
  , requireTraining = False
  , ability         = STR
  }

survie :: Skill
survie = Skill
  { skName          = "Survie"
  , requireTraining = False
  , ability         = WIS
  }

premiersSecours :: Skill
premiersSecours = Skill
  { skName          = "Premiers secours"
  , requireTraining = False
  , ability         = WIS
  }
