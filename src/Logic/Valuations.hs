{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Logic.Valuations where

import Data.Aeson (ToJSON, FromJSON, Object, Value(..))
import Data.Aeson.Key (Key, toText)
import Data.Aeson.KeyMap (toHashMap)
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Random (Random)
import qualified System.Random as SR

import Text.Blaze.Html.Renderer.Text ( renderHtml )

import Import.NoFoundation (shamlet, toStrict)

import Logic.Formulas
    ( Formula,
      GenFormula(..),
      BinaryConnective(..),
      UnaryConnective(..),
      NullaryConnective(..),
      Atomic(..), displayAtomic )

-- SECTION: Structures and valuations

class ValStructure v where
  evalNullary :: NullaryConnective -> v
  evalUnary   :: UnaryConnective   -> v -> v
  evalBinary  :: BinaryConnective  -> v -> v -> v

type Valuation v = Map Atomic v

-- | class for displaying and reading values
-- | laws:
-- | valFromDisplay . displayVal = Just
-- | if there is no v with displayVal v = str, then valFromDisplay str = Nothing
class ValDisplay v where
  displayVal :: v -> Text
  valFromDisplay :: Text -> Maybe v

-- | Given a Valuation on a ValStructure,
-- evaluate a Formula to a value
-- if all the atoms in the Formula are in the Valuation
eval :: ValStructure v
     => Valuation v
     -> Formula
     -> Maybe v
eval val (A at) = Map.lookup at val
eval _   (N nc) = Just $ evalNullary nc
eval val (U uc g) = evalUnary uc <$> (eval val g)
eval val (B bc g h) = evalBinary bc <$> (eval val g) <*> (eval val h)

-- | HTML table for a valuation
displayValuationHtml :: ValDisplay v => Valuation v -> Text
displayValuationHtml vln = toStrict . renderHtml $
  [shamlet|
          <table>
            <thead>
              <tr>
                <th>Atom
                <th>Value
            <tbody>
              $forall vpr <- Map.toAscList vln
                <tr>
                  <td .oblang>#{displayAtomic $ fst vpr}
                  <td .oblang>#{displayVal $ snd vpr}
  |]



-- | Read the reported valuation from an aeson Object
-- | Object is HashMap Text Value
-- | Value has several constructors, with the one we care about being String
objToValuation :: forall v. (ValDisplay v) => Object -> Valuation v
objToValuation obj = HashMap.foldrWithKey' inserter Map.empty (toHashMap obj)
  where
    inserter
      :: Key         -- ^ Text to use to form an Atomic, if a v can be determined
      -> Value        -- ^ aeson Value; might contain a Text that might determine a v
      -> Valuation v  -- ^ accumulator
      -> Valuation v
    inserter ky v acc =
      case v of
        String vtx -> -- the Value is a Text
          case valFromDisplay vtx of
            Nothing -> acc  -- the Text can't be read as a v
            Just vlu -> Map.insert (At (toText ky)) vlu acc  -- the Text can be read as a v; add entry to the Valuation v
        _ -> acc      -- the Value is not a Text

-- SECTION: Particular structures

-- Subsection: Boolean Structure

data BooleanStructure = BoolF | BoolT
  deriving (Read, Show, Eq, Enum, Bounded, Generic)

instance ToJSON BooleanStructure
instance FromJSON BooleanStructure

instance ValStructure BooleanStructure where
  evalNullary Verum  = BoolT
  evalNullary Falsum = BoolF

  evalUnary Negation BoolT = BoolF
  evalUnary Negation BoolF = BoolT

  evalBinary Conjunction BoolT BoolT = BoolT
  evalBinary Conjunction _     _     = BoolF
  evalBinary Disjunction BoolF BoolF = BoolF
  evalBinary Disjunction _     _     = BoolT
  evalBinary Implication BoolT BoolF = BoolF
  evalBinary Implication _     _     = BoolT

instance ValDisplay BooleanStructure where
  displayVal BoolT = "T"
  displayVal BoolF = "F"

  valFromDisplay = \case "T" -> Just BoolT
                         "F" -> Just BoolF
                         _   -> Nothing

instance Random BooleanStructure where
  randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
  random g = SR.randomR (minBound, maxBound) g

-- Subsection: Strong Kleene structure

data StrongKleeneStructure = StrongKleeneF | StrongKleeneStar | StrongKleeneT
  deriving (Read, Show, Eq, Enum, Bounded, Ord, Generic)

instance ToJSON StrongKleeneStructure
instance FromJSON StrongKleeneStructure

instance ValDisplay StrongKleeneStructure where
  displayVal StrongKleeneT = "T"
  displayVal StrongKleeneStar = "*"
  displayVal StrongKleeneF = "F"

  valFromDisplay = \case "T" -> Just StrongKleeneT
                         "*" -> Just StrongKleeneStar
                         "F" -> Just StrongKleeneF
                         _   -> Nothing

instance ValStructure StrongKleeneStructure where
  evalNullary Verum  = StrongKleeneT
  evalNullary Falsum = StrongKleeneF

  evalUnary Negation StrongKleeneT = StrongKleeneF
  evalUnary Negation StrongKleeneStar = StrongKleeneStar
  evalUnary Negation StrongKleeneF = StrongKleeneT

  evalBinary Conjunction u v = minimum [u, v]
  evalBinary Disjunction u v = maximum [u, v]
  evalBinary Implication u v = evalBinary Disjunction (evalUnary Negation u) v

instance Random StrongKleeneStructure where
  randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
  random g = SR.randomR (minBound, maxBound) g

-- Subsection: Dunn-Belnap structure

data DunnBelnapStructure = DunnBelnapF | DunnBelnapN | DunnBelnapB | DunnBelnapT
  deriving (Read, Show, Eq, Enum, Bounded, Generic)

instance ToJSON DunnBelnapStructure
instance FromJSON DunnBelnapStructure

instance ValDisplay DunnBelnapStructure where
  displayVal = \case DunnBelnapF -> "F"
                     DunnBelnapN -> "N"
                     DunnBelnapB -> "B"
                     DunnBelnapT -> "T"
  valFromDisplay = \case "F" -> Just DunnBelnapF
                         "N" -> Just DunnBelnapN
                         "B" -> Just DunnBelnapB
                         "T" -> Just DunnBelnapT
                         _   -> Nothing



instance ValStructure DunnBelnapStructure where
  evalNullary = \case Verum  -> DunnBelnapT
                      Falsum -> DunnBelnapF

  evalUnary Negation = \case DunnBelnapT -> DunnBelnapF
                             DunnBelnapN -> DunnBelnapN
                             DunnBelnapB -> DunnBelnapB
                             DunnBelnapF -> DunnBelnapT

  evalBinary Conjunction DunnBelnapT v = v
  evalBinary Conjunction u DunnBelnapT = u
  evalBinary Conjunction DunnBelnapF _ = DunnBelnapF
  evalBinary Conjunction _ DunnBelnapF = DunnBelnapF
  evalBinary Conjunction u v
    | v == u = u
    | otherwise = DunnBelnapF

  evalBinary Disjunction DunnBelnapF v = v
  evalBinary Disjunction u DunnBelnapF = u
  evalBinary Disjunction DunnBelnapT _ = DunnBelnapT
  evalBinary Disjunction _ DunnBelnapT = DunnBelnapT
  evalBinary Disjunction u v
    | v == u = u
    | otherwise = DunnBelnapT

  evalBinary Implication u v = evalBinary Disjunction (evalUnary Negation u) v

instance Random DunnBelnapStructure where
  randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
  random g = SR.randomR (minBound, maxBound) g

