{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Binchicken where

import Data.Text (Text)

import ExerciseType
  (ExerciseType(..)
  , ExerciseTargets(..)
  , prettyExerciseName
  )
import Logic.Formulas
  ( Atomic(..)
  , NullaryConnective(..)
  , UnaryConnective(..)
  , BinaryConnective(..)
  , Connective(..)
  , atomic)
import Foundation (Route(..), BinChicken)


-- | Settings for rolling a random formula
-- | May cause errors if things aren't sensible
data RandomFormulaSettings =
  RandomFormulaSettings
    { rfDegreeWeights :: [Double]  -- ^ picks a degree for the formula to have: eg [1, 2, 3] has 1/6 chance for degree 0, 2/6 for 1, 3/6 for 2
    , rfAtomics       :: [Atomic]
    , rfNullaryConns  :: [NullaryConnective]
    , rfUnaryConns    :: [UnaryConnective]
    , rfBinaryConns   :: [BinaryConnective]
    }


defRandomFormulaSettings :: RandomFormulaSettings
defRandomFormulaSettings =
  RandomFormulaSettings
        { rfDegreeWeights = [1, 2, 3, 2, 1]
        , rfAtomics = map atomic ['a'..'z']
        , rfNullaryConns = [Falsum, Verum]
        , rfUnaryConns = [Negation]
        , rfBinaryConns = [Conjunction, Disjunction, Implication]
        }

defProofRandomFormulaSettings :: RandomFormulaSettings
defProofRandomFormulaSettings =
  defRandomFormulaSettings

defEvalRandomFormulaSettings :: RandomFormulaSettings
defEvalRandomFormulaSettings =
  defRandomFormulaSettings { rfDegreeWeights = [0, 0, 3, 3, 2, 1] }

-- allow varying defaults by exercise type
defaultRandomFormulaSettings :: ExerciseType -> RandomFormulaSettings
defaultRandomFormulaSettings = \case
  DummyExercise                -> defRandomFormulaSettings
  IdentifyMainConnective       ->
    defRandomFormulaSettings { rfDegreeWeights = [1, 1, 5, 5, 4, 3] }
  EvaluateBoolean              -> defEvalRandomFormulaSettings
  EvaluateStrongKleene         -> defEvalRandomFormulaSettings
  EvaluateDunnBelnap           -> defEvalRandomFormulaSettings
  CounterexampleClassical      -> defRandomFormulaSettings
  CounterexampleNonclassical   -> defRandomFormulaSettings
  ProofWithRequirements        -> defRandomFormulaSettings
  ProveAnArgument              ->
    defRandomFormulaSettings { rfDegreeWeights = [1, 2] }
  AlphaEquivalence             -> defRandomFormulaSettings
  BetaReduction                -> defRandomFormulaSettings
  PerformSubstitution          ->
    defRandomFormulaSettings { rfAtomics = map atomic ['p'..'u'] }
  IntCounterexample            -> defRandomFormulaSettings



data RandomArgumentSettings =
  RandomArgumentSettings
    { raMaxDegree   :: Int -- ^ The degree of an argument is the sum of degrees of its formulas
    , raMaxPremises :: Int -- ^ Maximum number of premises
    , raMinPremises :: Int -- ^ Minimum number of premises
    , rarfSettings  :: RandomFormulaSettings -- ^ settings for each formula. Make sure that *both* raMaxDegree and the rfMaxDegree in here will be respected
    }

defRandomArgumentSettings :: RandomArgumentSettings
defRandomArgumentSettings =
  RandomArgumentSettings
    { raMaxDegree = 10
    , raMaxPremises = 3
    , raMinPremises = 0
    , rarfSettings = defRandomFormulaSettings { rfAtomics = map atomic ['p'..'t'] }
    }

defaultRandomArgumentSettings :: ExerciseType -> RandomArgumentSettings
defaultRandomArgumentSettings etype =
  defRandomArgumentSettings
    { rarfSettings = defaultRandomFormulaSettings etype }

raAtomics :: RandomArgumentSettings -> [Atomic]
raAtomics = rfAtomics . rarfSettings

data RandomSubstitutionSettings =
  RandomSubstitutionSettings
    { rsAtomics :: [Atomic] -- ^ which atomics to include in the substitution
    , rsIdPercent :: Int -- ^ percent chance to leave an atomic alone
    , rsRfSettings :: RandomFormulaSettings -- ^ settings to use to generate new substitutends 
    }

defRandomSubstitutionSettings :: RandomSubstitutionSettings
defRandomSubstitutionSettings =
  RandomSubstitutionSettings
    { rsAtomics = map atomic ['p'..'s']
    , rsIdPercent = 10
    , rsRfSettings = defaultRandomFormulaSettings PerformSubstitution
    }

-- | For counting rules, initial sequents count as 0
data RandomSequentPreProofSettings =
  RandomSequentPreProofSettings
    { rsppMaxRules :: Int -- ^ Maximum number of rules
    , rsppMinRules :: Int -- ^ Minimum number of rules
    , rsppraSettings :: RandomArgumentSettings -- ^ Settings to be obeyed by the *initial* sequents
    }

setRuleNum :: Int -> RandomSequentPreProofSettings -> RandomSequentPreProofSettings
setRuleNum r setts = setts { rsppMaxRules = r, rsppMinRules = r }

defRandomSequentPreProofSettings :: RandomSequentPreProofSettings
defRandomSequentPreProofSettings =
  RandomSequentPreProofSettings
    { rsppMaxRules = 8
    , rsppMinRules = 3
    , rsppraSettings = defRandomArgumentSettings
    }

setMaxComplexity :: Int -> RandomSequentPreProofSettings -> RandomSequentPreProofSettings
setMaxComplexity d setts =
  setts {
    rsppraSettings = (rsppraSettings setts) {
        rarfSettings = (rarfSettings $ rsppraSettings setts) {
            rfDegreeWeights = take d (rfDegreeWeights . rarfSettings . rsppraSettings $ setts)  } } }

setMaxPremises :: Int -> RandomSequentPreProofSettings -> RandomSequentPreProofSettings
setMaxPremises p setts =
  setts { rsppraSettings =
            (rsppraSettings setts) { raMaxPremises = p } }


data NormalizeRequirementSettings =
  NormalizeRequirementSettings
    { nrConnectives :: [Connective]
    , nrrfConclusion :: RandomFormulaSettings
    , nrMinLength :: Int
    , nrMaxLength :: Int
    }

defNormalizeRequirementSettings :: NormalizeRequirementSettings
defNormalizeRequirementSettings =
  NormalizeRequirementSettings
    { nrConnectives = [CB Conjunction, CB Disjunction, CB Implication, CU Negation]
    , nrrfConclusion = defProofRandomFormulaSettings
    , nrMinLength = 1
    , nrMaxLength = 3
    }

data RandomIntValidArgSettings =
  RandomIntValidArgSettings
    { idProbability :: Int       -- ^ 0 to 99, percent chance that each axiom is ID rather than FE/VI (with latter choice hardcoded 50/50 in randomAxiom)
    , kProbability :: Int        -- ^ 0 to 99, percent chance to weaken a random formula into an axiom 
    , removeProbability :: Int   -- ^ 0 to 99, percent chance to remove a used premise (ie in additive conj L)
    , vacProbability :: Int      -- ^ 0 to 99, percent chance to vacuously discharge in mult. impl R
    , continueProbability :: Int -- ^ 0 to 99, percent chance to add complexity if max depth not yet met
    , maxRuleDepth :: Int        -- ^ what it says on the tin
    }

defRandomIntValidArgSettings :: RandomIntValidArgSettings
defRandomIntValidArgSettings =
  RandomIntValidArgSettings
    { idProbability = 95
    , kProbability = 10
    , removeProbability = 85
    , vacProbability = 15
    , continueProbability = 97
    , maxRuleDepth = 2
    }

targets :: ExerciseType -> ExerciseTargets
targets = \case
  DummyExercise                -> ExerciseTargets 20 50 15
  IdentifyMainConnective       -> ExerciseTargets 20 50 15
  EvaluateBoolean              -> ExerciseTargets 15 30 12
  EvaluateStrongKleene         -> ExerciseTargets 15 30 12
  EvaluateDunnBelnap           -> ExerciseTargets 15 30 12
  CounterexampleClassical      -> ExerciseTargets 20 40 15
  CounterexampleNonclassical   -> ExerciseTargets 20 40 15
  ProofWithRequirements        -> ExerciseTargets 10 20 10
  ProveAnArgument              -> ExerciseTargets 10 20 12
  AlphaEquivalence             -> ExerciseTargets 10 20 12
  BetaReduction                -> ExerciseTargets 10 20 12
  PerformSubstitution          -> ExerciseTargets 15 30 12
  IntCounterexample            -> ExerciseTargets 15 30 12

fullStreak :: ExerciseType -> Int
fullStreak = streakMilestone1 . targets

exerciseRoute :: ExerciseType -> Route BinChicken
exerciseRoute = \case
  DummyExercise                -> DummyExerciseR
  IdentifyMainConnective       -> MainConnectiveR
  EvaluateBoolean              -> EvalBooleanR
  EvaluateStrongKleene         -> EvalStrongKleeneR
  EvaluateDunnBelnap           -> EvalDunnBelnapR
  CounterexampleClassical      -> CounterexClassicalR
  CounterexampleNonclassical   -> CounterexNonclassicalR
  ProofWithRequirements        -> ProofRequirementsR
  ProveAnArgument              -> ProveArgumentR
  AlphaEquivalence             -> AlphaEquivalenceR
  BetaReduction                -> BetaReductionR
  PerformSubstitution          -> PerformSubstitutionR
  IntCounterexample            -> IntModelsR

data ActiveET
  = Placeholder
  | Active ExerciseType
  deriving (Eq, Show)

-- | List of exercise types in actual use
activeExerciseTypes :: [ActiveET]
activeExerciseTypes =
  [ Active IdentifyMainConnective
  , Active PerformSubstitution
  , Active EvaluateBoolean
  , Active EvaluateStrongKleene
  , Active EvaluateDunnBelnap
  , Active CounterexampleClassical
  , Active CounterexampleNonclassical
  , Active ProveAnArgument
  ]


renderAET:: ActiveET -> (Text, Maybe ExerciseType)
renderAET aet =
  case aet of
    Placeholder -> ("Exercise not yet available", Nothing)
    Active et   -> (prettyExerciseName et, Just et)

rawActiveExerciseTypes :: [ExerciseType]
rawActiveExerciseTypes = go activeExerciseTypes
  where
    go [] = []
    go (Placeholder:xs) = go xs
    go (Active et:xs) = et : go xs
