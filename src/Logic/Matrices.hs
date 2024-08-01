{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Matrices where

import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text, intercalate)
import GHC.Generics (Generic)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Import.NoFoundation (shamlet, toStrict)


import Logic.Arguments ( Argument(..), atomsInArg )
import Logic.Formulas (Atomic(..), Formula, displayFormula)
import Logic.Valuations
    ( DunnBelnapStructure(..),
      StrongKleeneStructure(..),
      BooleanStructure(..),
      Valuation,
      ValDisplay(..),
      ValStructure(..),
      displayValuationHtml,
      eval )


-- SECTION: Matrices

class ValStructure m => ValMatrix m where
  isDesignated :: m -> Bool

-- | if a valuation is not a counterexample, encode why: undesignated premises and designated conclusion
data IsCounterexample = Counterexamples | UPsAndDCs [Formula] (Maybe Formula)
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data IsValid m = Valid | HasCounterexample (Valuation m)
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

-- | determine if a valuation is a counterexample to an argument
-- | Nothing returned if valuation doesn't cover all atomics in the argument
isCex :: ValMatrix m
      => Valuation m
      -> Argument
      -> Maybe IsCounterexample
isCex val (Argument prems conc) =
    case result of
      Nothing -> Nothing
      Just (udps, dc) ->
        if udps == [] && isNothing dc
        then Just Counterexamples
        else Just $ UPsAndDCs udps dc
    where
      resultUDPrems :: [Formula] -> Maybe [Formula]
      resultUDPrems [] = Just []
      resultUDPrems (p:ps) = case eval val p of
                                Nothing  -> Nothing
                                Just vlu -> if isDesignated vlu
                                            then resultUDPrems ps
                                            else (p : ) <$> resultUDPrems ps
      resultDConc :: Maybe (Maybe Formula)
      resultDConc = case eval val conc of
                      Nothing  -> Nothing
                      Just vlu -> if isDesignated vlu
                                  then Just (Just conc)
                                  else Just Nothing
      result :: Maybe ([Formula], Maybe Formula)
      result = case (resultUDPrems prems, resultDConc) of
                 (Just ups, Just dc) -> Just (ups, dc)
                 _                   -> Nothing


-- | Given a list of atomics, returns a list of all possible valuations on these atomics
allVals :: (ValStructure m, Bounded m, Enum m)
        => [Atomic]
        -> [Valuation m]
allVals [] = [Map.empty]
allVals (a:as) = [ Map.insert a vlu valn | vlu <- [minBound..maxBound], valn <- allVals as ]

-- | Given an argument, says whether the argument is valid, providing a counterexample if not
-- | Does not look past the first counterexample it finds; not guaranteed to find any particular one
isValid :: (ValMatrix m, Bounded m, Enum m)
        => Argument
        -> IsValid m
isValid arg = checker $ allVals (atomsInArg arg)
  where
    checker [] = Valid
    checker (v:vs) = case isCex v arg of
                        Nothing -> error "Shouldn't be here!"
                        Just Counterexamples -> HasCounterexample v
                        Just (UPsAndDCs _ _) -> checker vs

displayUPsHTML :: [Formula] -> Text
displayUPsHTML [] = ""
displayUPsHTML [p] = toStrict . renderHtml $
  [shamlet|
     <p>The premise
        <span .oblang> #{displayFormula p}
        takes an undesignated value on this valuation. In a counterexample to an argument, all premises must take designated values.
  |]
displayUPsHTML ups = toStrict . renderHtml $
  [shamlet|
     <p>The premises
        <span .oblang> #{intercalate ", " (map displayFormula ups)}
        take undesignated values on this valuation. In a counterexample to an argument, all premises must take designated values.
  |]

displayDCHTML :: Maybe Formula -> Text
displayDCHTML Nothing = ""
displayDCHTML (Just dc) = toStrict . renderHtml $
  [shamlet|
     <p>The conclusion
        <span .oblang> #{displayFormula dc}
        takes a designated value on this valuation. In a counterexample to an argument, the argument's conclusion must take an undesignated value.
  |]


-- SECTION: Instances

data MatrixTag =
    ClassicalTag
  | K3Tag
  | LPTag
  | FDETag
  deriving (Read, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

displayNameMatrix :: MatrixTag -> Text
displayNameMatrix =
  \case ClassicalTag -> "CL matrix"
        K3Tag -> "K3 matrix"
        LPTag -> "LP matrix"
        FDETag -> "FDE matrix"

newtype ClassicalMatrix = CM BooleanStructure
  deriving (Read, Show, Eq, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype  (ValStructure, ValDisplay)

instance ValMatrix ClassicalMatrix where
  isDesignated (CM BoolT) = True
  isDesignated _ = False

newtype K3Matrix = K3M StrongKleeneStructure
  deriving (Read, Show, Eq, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype  (ValStructure, ValDisplay)

instance ValMatrix K3Matrix where
  isDesignated (K3M StrongKleeneT) = True
  isDesignated _ = False

newtype LPMatrix = LPM StrongKleeneStructure
  deriving (Read, Show, Eq, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype  (ValStructure, ValDisplay)

instance ValMatrix LPMatrix where
  isDesignated (LPM StrongKleeneF) = False
  isDesignated _ = True

newtype FDEMatrix = FDEM DunnBelnapStructure
  deriving (Read, Show, Eq, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype  (ValStructure, ValDisplay)

instance ValMatrix FDEMatrix where
  isDesignated (FDEM DunnBelnapT) = True
  isDesignated (FDEM DunnBelnapB) = True
  isDesignated _ = False


-- SECTION : Existential types for working with unknown matrices

data MatrixInfo =
  forall v . (ValDisplay v, ValMatrix v)
    => MatrixInfo { matrixVals :: [v]
                  , matrixTag :: MatrixTag
                  }

matrixCL :: MatrixInfo
matrixCL = MatrixInfo [minBound..maxBound :: ClassicalMatrix] ClassicalTag

matrixK3 :: MatrixInfo
matrixK3 = MatrixInfo [minBound..maxBound :: K3Matrix] K3Tag

matrixLP :: MatrixInfo
matrixLP = MatrixInfo [minBound..maxBound :: LPMatrix] LPTag

matrixFDE :: MatrixInfo
matrixFDE = MatrixInfo [minBound..maxBound :: FDEMatrix] FDETag

data MysteryMatrixValuation =
    MClassical (Valuation ClassicalMatrix)
  | MK3 (Valuation K3Matrix)
  | MLP (Valuation LPMatrix)
  | MFDE (Valuation FDEMatrix)
  deriving (Generic)

instance ToJSON MysteryMatrixValuation
instance FromJSON MysteryMatrixValuation

displayMMVHtml :: MysteryMatrixValuation -> Text
displayMMVHtml (MClassical v) = displayValuationHtml v
displayMMVHtml (MK3 v) = displayValuationHtml v
displayMMVHtml (MLP v) = displayValuationHtml v
displayMMVHtml (MFDE v) = displayValuationHtml v


isCexMV :: MysteryMatrixValuation -> Argument -> Maybe IsCounterexample
isCexMV (MClassical cv) = isCex cv
isCexMV (MK3 kv)        = isCex kv
isCexMV (MLP lpv)       = isCex lpv
isCexMV (MFDE fdev)     = isCex fdev

data IsValidMV = ValidMV | HasCounterexampleMV MysteryMatrixValuation
  deriving (Generic)

instance ToJSON IsValidMV
instance FromJSON IsValidMV

mystifyC :: IsValid ClassicalMatrix -> IsValidMV
mystifyC Valid = ValidMV
mystifyC (HasCounterexample cc) = HasCounterexampleMV (MClassical cc)

mystifyK3 :: IsValid K3Matrix -> IsValidMV
mystifyK3 Valid = ValidMV
mystifyK3 (HasCounterexample ck) = HasCounterexampleMV (MK3 ck)

mystifyLP :: IsValid LPMatrix -> IsValidMV
mystifyLP Valid = ValidMV
mystifyLP (HasCounterexample cl) = HasCounterexampleMV (MLP cl)

mystifyFDE :: IsValid FDEMatrix -> IsValidMV
mystifyFDE Valid = ValidMV
mystifyFDE (HasCounterexample cf) = HasCounterexampleMV (MFDE cf)

isValidMV :: MatrixTag -> Argument -> IsValidMV
isValidMV mtag arg = case mtag of
  ClassicalTag -> mystifyC (isValid arg)
  K3Tag        -> mystifyK3 (isValid arg)
  LPTag        -> mystifyLP (isValid arg)
  FDETag       -> mystifyFDE (isValid arg)
