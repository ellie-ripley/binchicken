{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Arguments where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (nub, sort)
import GHC.Generics (Generic)

import Logic.Formulas (Formula, Atomic, rawAtomsIn)

-- SECTION : Types and instances for arguments

-- | Single conclusion arguments, with a list of premises
-- not directly an instance of Random, since lists can have any length
data Argument = Argument [Formula] Formula
  deriving (Read, Show, Eq, Generic)

instance ToJSON Argument
instance FromJSON Argument

atomsInArg :: Argument -> [Atomic]
atomsInArg (Argument prems conc) = sort $ nub (rawAtomsIn conc ++ concatMap rawAtomsIn prems)
