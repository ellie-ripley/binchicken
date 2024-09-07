{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.DecideInt where

import Data.List (all, delete, elem, null)
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)

import Logic.Arguments
import Logic.Formulas
  ( Atomic(..)
  , NullaryConnective(..)
  , UnaryConnective(..)
  , BinaryConnective(..)
  , Connective(..)
  , GenFormula(..)
  , Formula
  , fum
  , isAtomic
  , mainConnective
  )
import Logic.Matrices
  ( ClassicalMatrix
  , IsValid(..)
  , isValid
  )


data Verdict =
    VerdictValid
  | VerdictInvalid
  deriving (Show, Eq, Generic)

-- LJT, rule by rule
-- each rule takes an argument as input
-- left rules also take an active formula to work on
-- return a list of arguments such that the original argument follows from this list via the rule


ljtConjLeft
  :: Argument   -- ^ argument to be decomposed
  -> Formula    -- ^ conjunction to decompose argument at
  -> Maybe [Argument]   -- ^ result of the decomposition
ljtConjLeft (Argument prems conc) cnj@(B Conjunction lc rc)
  | not $ cnj `elem` prems = Nothing
  | otherwise = Just [Argument (lc : rc : delete cnj prems) conc]
ljtConjLeft _ _ = Nothing

ljtConjRight
  :: Argument
  -> Maybe [Argument]
ljtConjRight (Argument prems (B Conjunction lc rc)) = Just [Argument prems lc, Argument prems rc]
ljtConjRight _ = Nothing

ljtDisjLeft
  :: Argument
  -> Formula
  -> Maybe [Argument]
ljtDisjLeft (Argument prems conc) dsj@(B Disjunction ld rd)
  | not $ dsj `elem` prems = Nothing
  | otherwise = let rest = delete dsj prems
                in Just [Argument (ld : rest) conc, Argument (rd : rest) conc]
ljtDisjLeft _ _ = Nothing

ljtDisjInlRight
  :: Argument
  -> Maybe [Argument]
ljtDisjInlRight (Argument prems (B Disjunction ld _)) = Just [Argument prems ld]
ljtDisjInlRight _ = Nothing

ljtDisjInrRight
  :: Argument
  -> Maybe [Argument]
ljtDisjInrRight (Argument prems (B Disjunction _ rd)) = Just [Argument prems rd]
ljtDisjInrRight _ = Nothing

ljtImplRight
  :: Argument
  -> Maybe [Argument]
ljtImplRight (Argument prems (B Implication ld rd)) =
  Just [Argument (ld : prems) rd]
ljtImplRight _ = Nothing

ljtImplLeftAtom
  :: Argument
  -> Formula
  -> Maybe [Argument]
ljtImplLeftAtom (Argument prems conc) imp@(B Implication (A at) cons)
  | not $ imp `elem` prems = Nothing
  | otherwise = Just [Argument (cons : A at : prems) conc]
ljtImplLeftAtom _ _ = Nothing

ljtImplLeftConj
  :: Argument
  -> Formula
  -> Maybe [Argument]
ljtImplLeftConj (Argument prems conc) imp@(B Implication (B Conjunction lc rc) cons)
  | not $ imp `elem` prems = Nothing
  | otherwise = Just [Argument (B Implication lc (B Implication rc cons) : prems) conc]
ljtImplLeftConj _ _ = Nothing

ljtImplLeftDisj
  :: Argument
  -> Formula
  -> Maybe [Argument]
ljtImplLeftDisj (Argument prems conc) imp@(B Implication (B Disjunction ld rd) cons)
  | not $ imp `elem` prems = Nothing
  | otherwise = Just [Argument (B Implication ld cons : B Implication rd cons : prems) conc]
ljtImplLeftDisj _ _ = Nothing

ljtImplLeftImpl
  :: Argument
  -> Formula
  -> Maybe [Argument]
ljtImplLeftImpl (Argument prems conc) imp@(B Implication (B Implication a c1) c2)
  | not $ imp `elem` prems = Nothing
  | otherwise = Just [Argument (B Implication c1 c2 : prems) (B Implication a c1), Argument (c2 : prems) conc]
ljtImplLeftImpl _ _ = Nothing


removeNegationFormula
  :: Formula
  -> Formula
removeNegationFormula (U Negation negatum) = B Implication negatum (N Falsum)
removeNegationFormula fm = fm

removeNegationArgument
  :: Argument
  -> Argument
removeNegationArgument (Argument prems conc) =
  Argument (map removeNegationFormula prems) (removeNegationFormula conc)


decideIntMain
  :: Argument
  -> Verdict
decideIntMain arg = decideIntuitionistic (removeNegationArgument arg)

-- | Decide classical validity of an argument
decideClassical
  :: Argument
  -> Verdict
decideClassical arg =
  case (isValid arg :: IsValid ClassicalMatrix) of
    Valid               -> VerdictValid
    HasCounterexample _ -> VerdictInvalid

-- | Decide intuitionistic validity of an argument.
-- Checks classical validity first in case that's a shortcut
decideIntuitionistic
  :: Argument
  -> Verdict
decideIntuitionistic arg =
  case decideClassical arg of
    VerdictInvalid -> VerdictInvalid
    VerdictValid   -> decideDyckhoff arg

-- | Uses Dyckhoff's LJT to decide intuitionistic validity.
decideDyckhoff
  :: Argument
  -> Verdict
decideDyckhoff (Argument prems conc)
  | conc `elem` prems     = VerdictValid   -- axiom Id
  | N Falsum `elem` prems = VerdictValid   -- axiom _|_L
  | otherwise = undefined   -- work through decomposition rules

-- ideas from https://malv.in/posts/2021-01-09-depth-first-and-breadth-first-search-in-haskell.html

expandArg
  :: Argument
  -> [[Argument]]
expandArg arg@(Argument prems conc)
  | conc `elem` prems = [[]]
  | fum `elem` prems = [[]]
  | otherwise = go prems []
  where
    go :: [Formula] -> [[Argument]] -> [[Argument]]
    go [] results =
      case conc of
        A _ -> results
        N Falsum -> results
        B Conjunction _ _ -> maybeToList (ljtConjRight arg) <> results
        B Disjunction _ _ -> maybeToList (ljtDisjInlRight arg) <> maybeToList (ljtDisjInrRight arg) <> results
        B Implication _ _ -> maybeToList (ljtImplRight arg) <> results
        U _ _ -> error "A negation made it to where it shouldn't!"
    go (p : ps) results =
      case p of
        A _ -> go ps results
        N Falsum -> [[]]
        B Conjunction _ _ -> maybeToList (ljtConjLeft arg p) <> go ps results
        B Disjunction _ _ -> maybeToList (ljtDisjLeft arg p) <> go ps results
        B Implication (A _) _ ->
          maybeToList (ljtImplLeftAtom arg p) <> go ps results
        B Implication (B Conjunction _ _) _ ->
          maybeToList (ljtImplLeftConj arg p) <> go ps results
        B Implication (B Disjunction _ _) _ ->
          maybeToList (ljtImplLeftDisj arg p) <> go ps results
        B Implication (B Implication _ _) _ ->
          maybeToList (ljtImplLeftImpl arg p) <> go ps results
        U _ _ -> error "A negation is hiding somewhere bad!"

-- | the key step: given a list of arguments where we want to see if they're all valid,
-- generate a list of lists of simpler arguments,
-- where (some (all isValid)) on the second list is equivalent to (all isValid) on the first
expand :: [Argument] -> [[Argument]]
expand [] = [[]]
expand (arg : args) = [ pa <> pargs | pa <- expandArg arg, pargs <- expand args ]

