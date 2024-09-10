{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.IntValid where

import Data.List (delete, nub)
import GHC.Generics (Generic)
import System.Random (Finite, Random(..), Uniform)

import Logic.Arguments
import Logic.Formulas

-- sequent rule by sequent rule
-- each takes premise arguments as inputs, together with other needed choices,
-- and produces the conclusion argument
-- each assumes that premise list is nubbed, and nubs its result in turn

data Remove =
    Remove
  | Keep
  deriving (Bounded, Enum, Eq, Read, Show)

data ComponentSide =
    LeftSide
  | RightSide
  deriving (Bounded, Enum, Eq, Finite, Generic, Read, Show, Uniform)

instance Random ComponentSide where
  randomR (lo, hi) g = let (ix, h) = randomR (fromEnum lo, fromEnum hi) g
                       in (toEnum ix, h)

data SequentRule =
    ACL
  | MCL
  | MCR
  | MDL
  | ADR
  | MIL
  | MIR
  | NL
  | NR
  deriving (Bounded, Enum, Eq, Finite, Generic, Read, Show, Uniform)

instance Random SequentRule where
  randomR (lo, hi) g = let (ix, h) = randomR (fromEnum lo, fromEnum hi) g
                       in (toEnum ix, h)

sequentRuleArity :: SequentRule -> Int
sequentRuleArity = \case
  ACL -> 1
  MCL -> 1
  MCR -> 2
  MDL -> 2
  ADR -> 1
  MIL -> 2
  MIR -> 1
  NL -> 1
  NR -> 1

sequentRulesWithArity :: Int -> [SequentRule]
sequentRulesWithArity 1 = [ACL, MCL, ADR, MIR, NL, NR]
sequentRulesWithArity 2 = [MCR, MDL, MIL]
sequentRulesWithArity _ = []

-- | auxiliary function that either deletes something or does nothing
remover
  :: Eq a
  => Remove -- ^ whether to delete the thing or do nothing
  -> a
  -> [a]
  -> [a]
remover Remove x xs = delete x xs
remover Keep   _ xs = xs

additiveConjLeft
  :: Argument -- ^ the premise argument
  -> Formula  -- ^ the conjunction to be introduced
  -> ComponentSide -- ^ are we using the left or right conjunct?
  -> Remove   -- ^ whether to remove that conjunct from the premises
  -> Maybe Argument
additiveConjLeft (Argument prems conc) cj@(B Conjunction lc rc) side remove =
  let activeConjunct = case side of
                         LeftSide -> lc
                         RightSide -> rc
      oldPremsToKeep = remover remove activeConjunct prems
  in if activeConjunct `elem` prems
     then Just $ Argument (cj : oldPremsToKeep) conc
     else Nothing
additiveConjLeft _ _ _ _ = Nothing

multiplicativeConjLeft
  :: Argument -- ^ the premise argument
  -> Formula  -- ^ the conjunction to be introduced
  -> Remove   -- ^ whether to remove the left conjunct
  -> Remove   -- ^ whether to remove the right conjunct
  -> Maybe Argument
multiplicativeConjLeft (Argument prems conc) cj@(B Conjunction lc rc) reml remr
  | not (lc `elem` prems && rc `elem` prems) = Nothing
  | otherwise = Just (Argument (cj : oldPremsToKeep) conc)
  where
    oldPremsToKeep = (remover reml lc) . (remover remr rc) $ prems
multiplicativeConjLeft _ _ _ _ = Nothing

-- no use for additive conj right, since we're building in contraction

-- | always succeeds!
multiplicativeConjRight
  :: Argument
  -> Argument
  -> Argument
multiplicativeConjRight (Argument ps1 c1) (Argument ps2 c2) =
  Argument (nub $ ps1 <> ps2) (conj c1 c2)

-- no use for additive disj left, since we're building in contraction

-- | not quite the standard multiplicative rule:
-- we here implicitly use \/R to get the RHSs to match, if needed
multiplicativeDisjLeft
  :: Argument -- ^ left premise argument
  -> Argument -- ^ right premise argument
  -> Formula  -- ^ formula to be introduced
  -> Remove   -- ^ whether to remove the left disjunct from the left argument
  -> Remove   -- ^ whether to remove the right disjunct from the right argument
  -> ComponentSide  -- ^ which disjunct of the conclusion comes from the left argument, if needed
  -> Maybe Argument
multiplicativeDisjLeft (Argument ps1 c1) (Argument ps2 c2) dj@(B Disjunction ld rd) reml remr side
  | not (ld `elem` ps1 && rd `elem` ps2) = Nothing
  | otherwise = Just $ Argument (nub $ dj : (lp <> rp)) conc
  where
    lp = remover reml ld ps1
    rp = remover remr rd ps2
    conc = if c1 == c2
           then c1
           else case side of
                  LeftSide  -> disj c1 c2
                  RightSide -> disj c2 c1
multiplicativeDisjLeft _ _ _ _ _ _ = Nothing


-- | always succeeds!
additiveDisjRight
  :: Argument -- ^ premise argument
  -> ComponentSide -- ^ which disjunct is already present?
  -> Formula  -- ^ the other disjunct
  -> Argument
additiveDisjRight (Argument prems conc) side od = Argument prems newConc
  where
    newConc = case side of
                LeftSide -> disj conc od
                RightSide -> disj od conc

-- | here we always remove, since keeping would make the result just a weakening of one of its premises
multiplicativeImplLeft
  :: Argument -- ^ left premise argument
  -> Argument -- ^ right premise argument
  -> Formula -- ^ which premise from the right argument is the consequent?
  -> Maybe Argument
multiplicativeImplLeft (Argument ps1 c1) (Argument ps2 c2) cons
  | not $ cons `elem` ps2 = Nothing
  | otherwise = Just $ Argument (nub $ activeFormula : oldPrems) c2
  where
    activeFormula = impl c1 cons
    oldPrems = ps1 <> delete cons ps2

-- | always succeeds!
multiplicativeImplRight
  :: Argument
  -> Formula -- ^ antecedent to introduce
  -> Remove -- ^ whether to remove it (if present)
  -> Argument
multiplicativeImplRight (Argument prems conc) ante remove = Argument newPrems (impl ante conc)
  where
    newPrems = remover remove ante prems

-- | always succeeds!
negationLeft
  :: Argument
  -> Argument
negationLeft (Argument prems conc) = Argument (nub $ neg conc : prems) fum

negationRight
  :: Argument
  -> Formula -- ^ premise to take the negation of
  -> Remove  -- ^ whether to remove it
  -> Maybe Argument
negationRight (Argument prems conc) fm remove
  | not $ fm `elem` prems = Nothing
  | not $ conc == fum = Nothing
  | otherwise = Just $ Argument (remover remove fm prems) (neg fm)
