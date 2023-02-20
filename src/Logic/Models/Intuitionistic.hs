{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Logic.Models.Intuitionistic where

import Data.Aeson (ToJSON, FromJSON)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

import Logic.Arguments (Argument(..))
import Logic.Formulas ( Atomic(..)
                      , NullaryConnective(..)
                      , UnaryConnective(..)
                      , BinaryConnective(..)
                      , Formula
                      , GenFormula(..)
                      , displayAtomic
                      )

type Point = Set Atomic
type Model = Set Point

displayModel
  :: Model
  -> Text
displayModel md = "{" <> inner <> "}"
  where inner = T.intercalate ", " pts
        pts = map displayPoint (S.toAscList md)
        displayPoint p = "{" <> innerP p <> "}"
        innerP p = T.intercalate ", " (map displayAtomic (S.toAscList p))

  

data IntValue = IntT | IntF
  deriving (Eq, Show)

everywhere :: (a -> Bool) -> Set a -> Bool
everywhere prd = foldr (\a b -> prd a && b) True

somewhere :: (a -> Bool) -> Set a -> Bool
somewhere prd = foldr (\a b -> prd a || b) False

-- | evaluate a formula at a point in a model. Returns Nothing iff the point isn't in the model
evalFormulaAtPoint
  :: Model
  -> Formula
  -> Point
  -> Maybe IntValue
evalFormulaAtPoint md fm pt
  | S.notMember pt md = Nothing
  | otherwise = Just $
        case fm of
          (A at)
            | S.member at pt -> IntT
            | otherwise -> IntF
          (N Falsum) -> IntF
          (N Verum)  -> IntT
          (U Negation neg)
            | everywhere
                (\p -> evalFormulaAtPoint md neg p == Just IntF)
                (S.filter (pt `S.isSubsetOf`) md) -> IntT
            | otherwise -> IntF
          (B Conjunction con1 con2)
            | evalFormulaAtPoint md con1 pt == Just IntT
                && evalFormulaAtPoint md con2 pt == Just IntT -> IntT
            | otherwise -> IntF
          (B Disjunction dis1 dis2)
            | evalFormulaAtPoint md dis1 pt == Just IntF
                && evalFormulaAtPoint md dis2 pt == Just IntF -> IntF
            | otherwise -> IntT
          (B Implication ante cons)
            | everywhere
                (\p -> evalFormulaAtPoint md ante p == Just IntF
                       || evalFormulaAtPoint md cons p == Just IntT)
                (S.filter (pt `S.isSubsetOf`) md) -> IntT
            | otherwise -> IntF

pointIsCounterexample
  :: Model
  -> Point
  -> Argument
  -> Maybe Bool
pointIsCounterexample md pt (Argument prems conc)
  | S.notMember pt md = Nothing
  | otherwise = Just $ all (\f -> evalFormulaAtPoint md f pt == Just IntT) prems
                        && evalFormulaAtPoint md conc pt == Just IntF

data ModelFeedback
  = IsCounterexample Model Argument
  | IsNotCounterexample Model Argument
  deriving (Generic, ToJSON, FromJSON)

modelIsCounterexample
  :: Model
  -> Argument
  -> ModelFeedback
modelIsCounterexample md arg
  | everywhere (\p -> pointIsCounterexample md p arg == Just False) md = IsNotCounterexample md arg
  | somewhere  (\p -> pointIsCounterexample md p arg == Just True)  md = IsCounterexample md arg
  | otherwise = error "Problem checking counterexample!"

type Parser = Parsec Void Text

preParse :: Text -> Text
preParse = T.filter (`elem` "{}" <> ['a'..'z'])

parsePoint :: Parser Point
parsePoint = do
  _   <- char '{'
  ats <- many lowerChar
  _   <- char '}'
  return $ S.fromList (map (At . T.singleton) ats)

parseModel :: Parser Model
parseModel = do
  _   <- char '{'
  pts <- many parsePoint
  _   <- char '}'
  return $ S.fromList pts
