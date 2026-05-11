{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Logic.Models.Intuitionistic where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.List as L
import qualified Data.Map as M
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


type RawFrame = M.Map Int (Set Int)
newtype Frame = Frame RawFrame
  deriving (Generic, ToJSON, FromJSON)
data IntModel
  = IntModel
  { frame :: !Frame
  , valuation :: !(M.Map Int (Set Atomic))
  }
  deriving (Generic, ToJSON, FromJSON)

addLinks :: [(Int, Int)] -> RawFrame -> RawFrame
addLinks links rf = foldl' (\acc lk -> addLink lk acc) rf links
  where addLink (u, v) r =
          case M.lookup u r of
            Nothing -> M.insert u (S.singleton v) r
            Just zs -> M.insert u (S.insert v zs) r

fixpoint
  :: Eq a
  => (a -> a)
  -> a
  -> a
fixpoint f x =
  let x' = f x
  in if x' == x then x else fixpoint f x'

allPointsRaw :: RawFrame -> Set Int
allPointsRaw = M.foldlWithKey' (\acc k s -> S.insert k (S.union s acc)) S.empty

reflClose :: RawFrame -> RawFrame
reflClose rf =
  let reflLinks = map (\k -> (k, k)) (S.toList $ allPointsRaw rf)
  in addLinks reflLinks rf

transStep :: RawFrame -> RawFrame
transStep rf = M.map (\ps -> S.foldl' (\acc n -> S.union acc (succs n)) ps ps) rf
  where
    succs :: Int -> Set Int
    succs m = M.findWithDefault S.empty m rf

transClose :: RawFrame -> RawFrame
transClose = fixpoint transStep
                            
-- | smart constructor: only outputs frames that really are preorders
listToFrame :: [(Int, Int)] -> Frame
listToFrame lks = Frame . reflClose . transClose $ addLinks lks M.empty

framePoints :: Frame -> Set Int
framePoints (Frame rf) = allPointsRaw rf
      
data IntValue = IntT | IntF
  deriving (Eq, Show)

everywhere :: (a -> Bool) -> Set a -> Bool
everywhere prd = foldr (\a b -> prd a && b) True

somewhere :: (a -> Bool) -> Set a -> Bool
somewhere prd = foldr (\a b -> prd a || b) False

succs :: Frame -> Int -> Set Int
succs (Frame rf) p = M.findWithDefault S.empty p rf 



-- | evaluate a formula at a point in a model. Crashes if point is not in the model.
evalFormulaAtPoint
  :: IntModel
  -> Formula
  -> Int
  -> IntValue
evalFormulaAtPoint md fm pt
  | not (S.member pt $ framePoints (frame md)) = error "Error 67. This is a bug in the site; please report it!"
  | otherwise =
      case fm of
            (A at)
                | S.member at (M.findWithDefault S.empty pt (valuation md)) -> IntT
                | otherwise -> IntF
            (N Verum) -> IntT
            (N Falsum) -> IntF
            (U Negation neg)
                | everywhere (\p -> evalFormulaAtPoint md neg pt == IntF) (succs (frame md) pt) -> IntT
                | otherwise -> IntF
            (B Conjunction c1 c2)
                | (evalFormulaAtPoint md c1 pt == IntT
                    && evalFormulaAtPoint md c2 pt == IntT) -> IntT
                | otherwise -> IntF
            (B Disjunction d1 d2)
                | (evalFormulaAtPoint md d1 pt == IntT
                    || evalFormulaAtPoint md d2 pt == IntT) -> IntT
                | otherwise -> IntF
            (B Implication an co)
                | everywhere (\p -> evalFormulaAtPoint md an p == IntF
                                || evalFormulaAtPoint md co p == IntT) (succs (frame md) pt) -> IntT
                | otherwise -> IntF

data PointCounterexample =
  PointCounterexample IntModel Int Argument
  deriving (Generic, ToJSON, FromJSON)

data PointFeedback
  = IsPointCounterexample PointCounterexample
  | UntruePremises IntModel Int Argument [Formula]
  | TrueConclusion IntModel Int Argument
  deriving (Generic, ToJSON, FromJSON)

isPointCounterexample :: PointFeedback -> Bool
isPointCounterexample =
  \case IsPointCounterexample _ -> True
        _ -> False

data ModelFeedback
  = IsModelCounterexample IntModel (Set Int) Argument
  | IsNotCounterexample IntModel Argument
  deriving (Generic, ToJSON, FromJSON)
          
pointCounterexample
  :: IntModel
  -> Int
  -> Argument
  -> PointFeedback
pointCounterexample md pt arg@(Argument prems conc)
  = let untruePrems = filter (\fm -> evalFormulaAtPoint md fm pt == IntF) prems
        concVal  = evalFormulaAtPoint md conc pt
    in  if null untruePrems
        then if concVal == IntF
                then IsPointCounterexample $ PointCounterexample md pt arg
                else TrueConclusion md pt arg
        else UntruePremises md pt arg untruePrems

modelCounterexample
  :: IntModel
  -> Argument
  -> ModelFeedback
modelCounterexample md arg =
  let counterPoints = S.filter (\p -> isPointCounterexample $ pointCounterexample md p arg)
                                (framePoints $ frame md)
  in  if S.null counterPoints
         then IsNotCounterexample md arg
         else IsModelCounterexample md counterPoints arg
        
type Point' = Set Atomic
type Model = Set Point'

displayModel
  :: Model
  -> Text
displayModel md = "{" <> inner <> "}"
  where inner = T.intercalate ", " pts
        pts = map displayPoint' (S.toAscList md)
        displayPoint' p = "{" <> innerP p <> "}"
        innerP p = T.intercalate ", " (map displayAtomic (S.toAscList p))

  


-- | evaluate a formula at a point in a model. Returns Nothing iff the point isn't in the model
evalFormulaAtPoint'
  :: Model
  -> Formula
  -> Point'
  -> Maybe IntValue
evalFormulaAtPoint' md fm pt
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
                (\p -> evalFormulaAtPoint' md neg p == Just IntF)
                (S.filter (pt `S.isSubsetOf`) md) -> IntT
            | otherwise -> IntF
          (B Conjunction con1 con2)
            | evalFormulaAtPoint' md con1 pt == Just IntT
                && evalFormulaAtPoint' md con2 pt == Just IntT -> IntT
            | otherwise -> IntF
          (B Disjunction dis1 dis2)
            | evalFormulaAtPoint' md dis1 pt == Just IntF
                && evalFormulaAtPoint' md dis2 pt == Just IntF -> IntF
            | otherwise -> IntT
          (B Implication ante cons)
            | everywhere
                (\p -> evalFormulaAtPoint' md ante p == Just IntF
                       || evalFormulaAtPoint' md cons p == Just IntT)
                (S.filter (pt `S.isSubsetOf`) md) -> IntT
            | otherwise -> IntF

pointIsCounterexample'
  :: Model
  -> Point'
  -> Argument
  -> Maybe Bool
pointIsCounterexample' md pt (Argument prems conc)
  | S.notMember pt md = Nothing
  | otherwise = Just $ all (\f -> evalFormulaAtPoint' md f pt == Just IntT) prems
                        && evalFormulaAtPoint' md conc pt == Just IntF



type Parser = Parsec Void Text

preParse :: Text -> Text
preParse = T.filter (`elem` "{}" <> ['a'..'z'])

parsePoint' :: Parser Point'
parsePoint' = do
  _   <- char '{'
  ats <- many lowerChar
  _   <- char '}'
  return $ S.fromList (map (At . T.singleton) ats)

parseModel :: Parser Model
parseModel = do
  _   <- char '{'
  pts <- many parsePoint'
  _   <- char '}'
  return $ S.fromList pts
