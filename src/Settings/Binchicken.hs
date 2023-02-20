{-# LANGUAGE LambdaCase #-}

module Settings.Binchicken where

import ExerciseType (ExerciseType(..), ExerciseTargets(..))
import Logic.Formulas (Atomic(..), NullaryConnective(..), UnaryConnective(..), BinaryConnective(..), Connective(..), atomic)
import Foundation (Route(..), BinChicken)


-- | Settings for rolling a random formula
-- | May cause errors if things aren't sensible
data RandomFormulaSettings =
  RandomFormulaSettings
    { rfMaxDegree    :: Int
    , rfMinDegree    :: Int
    , rfAtomics      :: [Atomic]
    , rfNullaryConns :: [NullaryConnective]
    , rfUnaryConns   :: [UnaryConnective]
    , rfBinaryConns  :: [BinaryConnective]
    }

defRandomFormulaSettings :: RandomFormulaSettings
defRandomFormulaSettings =
  RandomFormulaSettings
        { rfMaxDegree = 5
        , rfMinDegree = 0
        , rfAtomics = map atomic ['a'..'z']
        , rfNullaryConns = []
        , rfUnaryConns = [Negation]
        , rfBinaryConns = [Conjunction, Disjunction]
        }

defProofRandomFormulaSettings :: RandomFormulaSettings
defProofRandomFormulaSettings =
  defRandomFormulaSettings
    { rfNullaryConns = [Falsum]
    , rfBinaryConns = [Conjunction, Disjunction, Implication]
    }

-- allow varying defaults by exercise type
defaultRandomFormulaSettings :: ExerciseType -> RandomFormulaSettings
defaultRandomFormulaSettings = \case
  DummyExercise                -> defRandomFormulaSettings
  IdentifyMainConnective       -> defRandomFormulaSettings
  EvaluateBoolean              -> defRandomFormulaSettings
  EvaluateStrongKleene         -> defRandomFormulaSettings
  EvaluateDunnBelnap           -> defRandomFormulaSettings
  CounterexampleClassical      -> defRandomFormulaSettings
  CounterexampleNonclassical   -> defRandomFormulaSettings
  ProofWithRequirements        -> defProofRandomFormulaSettings
  ProofIntuitionistic          -> defProofRandomFormulaSettings
  ProofNormalize               -> defProofRandomFormulaSettings
  CounterexampleIntuitionistic -> defProofRandomFormulaSettings


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
  setts { rsppraSettings =
            (rsppraSettings setts) { rarfSettings =
                                      (rarfSettings $ rsppraSettings setts) { rfMaxDegree = d } } }

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

targets :: ExerciseType -> ExerciseTargets
targets = \case
  DummyExercise                -> ExerciseTargets 20 50 15
  IdentifyMainConnective       -> ExerciseTargets 20 50 15
  EvaluateBoolean              -> ExerciseTargets 15 30 10
  EvaluateStrongKleene         -> ExerciseTargets 15 30 10
  EvaluateDunnBelnap           -> ExerciseTargets 15 30 10
  CounterexampleClassical      -> ExerciseTargets 20 40 15
  CounterexampleNonclassical   -> ExerciseTargets 20 40 15
  ProofWithRequirements        -> ExerciseTargets 10 20 10
  ProofIntuitionistic          -> ExerciseTargets 15 30 10
  ProofNormalize               -> ExerciseTargets 10 20 5
  CounterexampleIntuitionistic -> ExerciseTargets 10 20 10

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
  ProofIntuitionistic          -> ProofIntuitionisticR
  ProofNormalize               -> ProofNormalizeR
  CounterexampleIntuitionistic -> CounterexIntuitionisticR

-- | List of exercise types in actual use
activeExerciseTypes :: [ExerciseType]
activeExerciseTypes =
  [ IdentifyMainConnective
  , EvaluateBoolean
  , EvaluateStrongKleene
  , EvaluateDunnBelnap
  , CounterexampleClassical
  , CounterexampleNonclassical
  , ProofWithRequirements
  , ProofIntuitionistic
  , ProofNormalize
  , CounterexampleIntuitionistic
  ]
