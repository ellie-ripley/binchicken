{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Formulas where

import Control.Monad (ap)

import Data.Bifunctor (first)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import System.Random (Random)
import qualified System.Random as SR
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import GHC.Generics ( Generic )

-- SECTION: types and instances for formulas

newtype Atomic = At Text deriving (Read, Show, Eq, Ord, Generic)

data NullaryConnective =
    Verum
  | Falsum
  deriving (Read, Show, Eq, Enum, Bounded, Generic)

data UnaryConnective =
    Negation
  deriving (Read, Show, Eq, Enum, Bounded, Generic)

data BinaryConnective =
    Conjunction
  | Disjunction
  | Implication
  deriving (Read, Show, Eq, Enum, Bounded, Generic)

data Connective =
    CN NullaryConnective
  | CU UnaryConnective
  | CB BinaryConnective
  deriving (Read, Show, Eq, Generic)

instance ToJSON Atomic
instance ToJSONKey Atomic
instance ToJSON NullaryConnective
instance ToJSON UnaryConnective
instance ToJSON BinaryConnective
instance ToJSON Connective

instance FromJSON Atomic
instance FromJSONKey Atomic
instance FromJSON NullaryConnective
instance FromJSON UnaryConnective
instance FromJSON BinaryConnective
instance FromJSON Connective

instance Enum Connective where
    toEnum i
        | i < unOffset  = CN (toEnum i)
        | i < biOffset  = CU (toEnum (i - unOffset))
        | i < triOffset = CB (toEnum (i - biOffset))
        | otherwise     = error "Not that many connectives!"
        where
            unOffset  = fromEnum (maxBound :: NullaryConnective) + 1
            biOffset  = fromEnum (maxBound :: UnaryConnective) + unOffset + 1
            triOffset = fromEnum (maxBound :: BinaryConnective) + biOffset + 1

    fromEnum = \case
        (CN n) -> fromEnum n
        (CU u) -> fromEnum u + unOffset
        (CB b) -> fromEnum b + biOffset
        where
            unOffset  = fromEnum (maxBound :: NullaryConnective) + 1
            biOffset  = fromEnum (maxBound :: UnaryConnective) + unOffset + 1

instance Bounded Connective where
    minBound = toEnum 0
    maxBound = toEnum maxI
        where
            maxI = fromEnum (maxBound :: NullaryConnective)
                 + fromEnum (maxBound :: UnaryConnective)
                 + fromEnum (maxBound :: BinaryConnective)
                 + 2

data GenFormula a =
    A a
  | N NullaryConnective
  | U UnaryConnective (GenFormula a)
  | B BinaryConnective (GenFormula a) (GenFormula a)
  deriving (Read, Show, Eq, Foldable, Functor, Generic)

instance (ToJSON a) => ToJSON (GenFormula a)
instance (FromJSON a) => FromJSON (GenFormula a)

type Formula = GenFormula Atomic

instance Applicative GenFormula where
    pure = return
    (<*>) = ap

instance Monad GenFormula where
    return = A
    f >>= fun =
      case f of
        (A a) -> fun a
        (N n) -> N n
        (U u g) -> U u (g >>= fun)
        (B b g h) -> B b (g >>= fun) (h >>= fun)

{-
Monad GenFormula is lawful:
    1 .pure a >>= k = k a
    (A a) >>= k = ka

    2. m >>= pure = m
    m >>= A = m

    3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
    case m:
        (A a) >>= (\x -> k x >>= h) =
        k a >>= h =
        ((A a) >>= k) >>= h

        (N n) >>= _ = N n = ((N n) >>= _) >>= _

        (U u f) >>= (\x -> k x >>= h) =
        U u (f >>= (\x -> k x >>= h) =IH
        U u ((f >>= k) >>= h) =
        (U u (f >>= k)) >>= h =
        ((U u f) >>= k) >>= h

        B just like U, but two at once
-}

instance Traversable GenFormula where
    sequenceA :: Applicative f => GenFormula (f a) -> f (GenFormula a)
    sequenceA (A a) = A <$> a
    sequenceA (N n) = pure (N n)
    sequenceA (U u f) = (U u) <$> (sequenceA f)
    sequenceA (B b f g) = (B b) <$> (sequenceA f) <*> (sequenceA g)

{-
Traversable GenFormula is lawful:
    an *AT* is a t :: (Applicative f, Applicative g) => f a -> g a such that
        AT1. t (pure x) = pure x
        AT2. t (f <*> x) = t f <*> t x

    AL1 here is (pure f) <*> x = f <$> x

    1. (AT t) => t . sequenceA = sequenceA . fmap t
    t (sequenceA g) = sequenceA (t <$> g)
    case g:
        t (sequenceA (A a)) =
        t (A <$> a) =AL1
        t ((pure A) <*> a) =AT2
        t (pure A) <*> t a =AT1
        pure A <*> t a =AL1
        A <$> t a =
        sequenceA (A (t a)) =
        sequenceA (fmap t (A a))

        t (sequenceA (N n)) =
        t (pure (N n)) =AT1
        pure (N n) =
        sequenceA (N n) =
        sequenceA (fmap t (N n))

        t (sequenceA (U u f)) =
        t ((U u) <$> (sequenceA f)) =AL1
        t ((pure $ U u) <*> (sequenceA f)) =AT2
        t (pure $ U u) <*> t (sequenceA f) =AT1, IH
        pure (U u) <*> ((sequenceA . fmap t) f) =AL1
        (U u) <$> ((sequenceA . fmap t) f) =
        (sequenceA . fmap t) (U u f)

        B like U

    2. sequenceA . fmap Identity = Identity
    obvious

    3. sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
    TODO
-}


-- random for Atomic always picks a lowercase single character
-- randomR ignores range!
instance Random Atomic where
    random g = first (At . textify) (SR.randomR ('a' :: Char, 'z') g)
        where
            textify c = T.pack (c:[])
    randomR _ g = SR.random g

instance Random NullaryConnective where
    randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
    random g = SR.randomR (minBound, maxBound) g

instance Random UnaryConnective where
    randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
    random g = SR.randomR (minBound, maxBound) g

instance Random BinaryConnective where
    randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
    random g = SR.randomR (minBound, maxBound) g

instance Random Connective where
    randomR (lo, hi) g = first toEnum (SR.randomR (fromEnum lo, fromEnum hi) g)
    random g = SR.randomR (minBound, maxBound) g

-- SECTION: rendering

displayNC :: NullaryConnective -> Text
displayNC Verum = "TROO"
displayNC Falsum = "_|_"

displayUC :: UnaryConnective -> Text
displayUC Negation = "~"

displayBC :: BinaryConnective -> Text
displayBC Conjunction = "/\\"
displayBC Disjunction = "\\/"
displayBC Implication = "->"

displayConnective :: Connective -> Text
displayConnective (CN n) = displayNC n
displayConnective (CU u) = displayUC u
displayConnective (CB b) = displayBC b

displayMaybeConnective :: Maybe Connective -> Text
displayMaybeConnective (Just c) = displayConnective c
displayMaybeConnective Nothing = "Nothing"

displayAtomic :: Atomic -> Text
displayAtomic (At t) = t

displayParens :: Text -> Text
displayParens t = "(" <> t <> ")"

parenComponent :: (a -> Text) -> GenFormula a -> Text
parenComponent f g@(B _ _ _) = displayParens (displayGenFormula f g)
parenComponent f h = displayGenFormula f h

displayGenFormula :: (a -> Text) -> GenFormula a -> Text
displayGenFormula f (A a) = f a
displayGenFormula _ (N n) = displayNC n
displayGenFormula f (U u g) =
    displayUC u <> parenComponent f g
displayGenFormula f (B b g h) =
    parenComponent f g
        <> " "
        <> displayBC b
        <> " "
        <> parenComponent f h

displayFormula :: Formula -> Text
displayFormula = displayGenFormula displayAtomic


-- SECTION: naming (for html tags and the like)
nameNC :: NullaryConnective -> Text
nameNC Verum = "verum"
nameNC Falsum = "falsum"

nameUC :: UnaryConnective -> Text
nameUC Negation = "negation"

nameBC :: BinaryConnective -> Text
nameBC Conjunction = "conjunction"
nameBC Disjunction = "disjunction"
nameBC Implication = "implication"

nameConnective :: Connective -> Text
nameConnective (CN n) = nameNC n
nameConnective (CU u) = nameUC u
nameConnective (CB b) = nameBC b

-- SECTION: helper functions

mainConnective :: (GenFormula a) -> Maybe Connective
mainConnective (A _)     = Nothing
mainConnective (N n)     = Just $ CN n
mainConnective (U u _)   = Just $ CU u
mainConnective (B b _ _) = Just $ CB b

atomic :: Char -> Atomic
atomic = At . T.pack . (:[])

atom :: Text -> Formula
atom = A . At

vum :: GenFormula a
vum = N Verum

fum :: GenFormula a
fum = N Falsum

neg :: GenFormula a -> GenFormula a
neg g = U Negation g

conj :: GenFormula a -> GenFormula a -> GenFormula a
conj g h = B Conjunction g h

disj :: GenFormula a -> GenFormula a -> GenFormula a
disj g h = B Disjunction g h

impl :: GenFormula a -> GenFormula a -> GenFormula a
impl g h = B Implication g h

rawAtomsIn :: GenFormula a -> [a]
rawAtomsIn (A a) = [a]
rawAtomsIn (N _) = []
rawAtomsIn (U _ f) = rawAtomsIn f
rawAtomsIn (B _ g h) = rawAtomsIn g ++ rawAtomsIn h

atomsIn :: (Eq a) => GenFormula a -> [a]
atomsIn = nub . rawAtomsIn

degree :: GenFormula a -> Int
degree = \case
  A _ -> 0
  N _ -> 1
  U _ g -> degree g + 1
  B _ g h -> degree g + degree h + 1
