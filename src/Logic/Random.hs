{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Random where

import Data.Bifunctor (first)
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE

import Logic.Formulas
import Logic.Arguments
import Logic.Valuations

import qualified Data.Map.Strict as Map

import System.Random (Random, RandomGen)
import qualified System.Random as SR
import Settings.Binchicken (RandomArgumentSettings(..), RandomFormulaSettings(..))


-- SECTION: Random formulas

-- | given a finite nonempty list of Doubles and a Double,
-- | gives the last coordinate in the list where the running sum
-- | remains below the Double
findCoord
  :: [Double]
  -> Double
  -> Int
findCoord [] _ = error "Shouldn't be here; a list was empty that needs entries!"
findCoord (y:ys) u = go (y:ys) u 0 0
  where
    go []     _ _ i = i
    go (x:xs) t s i =
      if s < t && s + x >= t
      then i
      else go xs t (s + x) (i + 1)

-- | picks a degree, following the specified weights
randomDegree
  :: (RandomGen g)
  => RandomFormulaSettings
  -> g
  -> (Int, g)
randomDegree setts g = (deg, h)
  where
    weights = rfDegreeWeights setts
    (ind, h) = SR.randomR (0, sum weights) g
    deg = findCoord weights ind

-- | Given a degree, makes a list of weights where all weight is on that degree
determinedDegree :: Int -> [Double]
determinedDegree n
  | n < 0  = error "No negative degrees!"
  | n == 0 = [1]
  | otherwise = 0 : determinedDegree (n - 1)



-- | Gives a random formula following all settings
randomFormula
  :: (RandomGen g)
  => RandomFormulaSettings  -- ^ Settings to use
  -> g
  -> (Formula, g)
randomFormula setts g =
      let (deg, g1) = randomDegree setts g
      in case deg of
           -- degree 0: pick an atomic from the list.
           0 -> let ats = rfAtomics setts
                    (ix, g2) = SR.randomR (0, length ats - 1) g1
                in (A $ ats !! ix, g2)
           -- degree d: pick a connective, divide if needed, and recurse
           d -> let posArConns = map CU (rfUnaryConns setts) ++ map CB (rfBinaryConns setts)
                    connList = if d == 1
                               then map CN (rfNullaryConns setts) ++ posArConns
                               else posArConns  -- can't pick nullary connective unless at degree 1
                    (ix, g2) = SR.randomR (0, length connList - 1) g1
                in case connList !! ix of
                     CN n -> (N n, g2)
                     CU u -> let newSetts = setts { rfDegreeWeights = determinedDegree (d - 1) }
                             in first (U u) $ randomFormula newSetts g2
                     CB b -> let (deg1, g3) = SR.randomR (0, d - 1) g2
                                 setts1 = setts { rfDegreeWeights = determinedDegree (deg1) }
                                 setts2 = setts { rfDegreeWeights = determinedDegree ((d - 1) - deg1) }
                                 (com1, g4) = randomFormula setts1 g3
                                 (com2, g5) = randomFormula setts2 g4
                             in (B b com1 com2, g5)

-- | Takes settings and a fixed degree, generates a formula of that exact degree
-- | Ignores what the settings say about degree; obeys the rest of the settings
randomFormulaFixedDegree
  :: (RandomGen g)
  => RandomFormulaSettings -- ^ Degree stuff in these settings is ignored!
  -> Int                   -- ^ Desired degree
  -> g
  -> (Formula, g)
randomFormulaFixedDegree setts d g =
  let newSetts = setts { rfDegreeWeights = determinedDegree d }
  in randomFormula newSetts g

randomFormulaIO :: RandomFormulaSettings -> IO Formula
randomFormulaIO setts = SR.getStdRandom $ randomFormula setts

-- | Gives a sized list of random formulas
randomFormulas
  :: (RandomGen g)
  => RandomFormulaSettings
  -> Int                    -- length of list to return
  -> g
  -> ([Formula], g)
randomFormulas setts n g
  | n < 0     = error "Shouldn't be here! Asked for a list with negative size"
  | n == 0    = ([], g)
  | otherwise = let (fms, h) = randomFormulas setts (n - 1) g
                    (newfm, i) = randomFormula setts h
                in (newfm : fms, i)




  -- SECTION: Random arguments

-- | Given a RandomArgumentSettings and a seed, produces a random argument in accord with those settings and a new seed
-- | Malformed settings: negative degree errors, negative minpremises set to 0, maxpremises < minpremises gets adjusted up to minpremises. No checks on RandomFormulaSettings; these are passed to the formula randomizing functions
randomArgument :: RandomGen g
               => RandomArgumentSettings
               -> g
               -> (Argument, g)
randomArgument setts g
  -- rule out negative degree
  | raMaxDegree setts < 0 = error "Negative degree!?"
  | otherwise =
      let minPs = max 0     (raMinPremises setts)
          maxPs = max minPs (raMaxPremises setts)
          maxDeg = raMaxDegree setts
          rfSetts = rarfSettings setts
          maxFDeg = length (rfDegreeWeights rfSetts)
          (numPrems, g1) = SR.randomR (minPs, maxPs) g -- ^ random number of premises
          (g2, h) = SR.split g1 -- ^ split generator so we can unfold one and throw it out
          degs = NE.unfoldr (go maxFDeg) (numPrems, maxDeg, g2)
          conDeg = NE.head degs
          premDegs = NE.tail degs
          (conc :: Formula, h1) = randomFormulaFixedDegree rfSetts conDeg h
          (prems :: [Formula], hfin) =
            foldl' (\(ps, i) d -> let (newP, i1) = randomFormulaFixedDegree rfSetts d i
                                  in  (newP : ps, i1))
                   ([], h1)
                   premDegs
      in (Argument prems conc, hfin)
      where go :: RandomGen g   -- dunno why this is needed
               => Int           -- max degree of each formula
               -> (Int, Int, g) -- premises remaining, total degree remaining, seed
               -> (Int, Maybe (Int, Int, g))
            go maxFDeg (nps, dr, g3) = let (newDeg, g4) = SR.randomR (0, (min dr $ maxFDeg)) g3
                                       in case nps of
                                            0 -> (newDeg, Nothing)
                                            n -> (newDeg, Just (n - 1, dr - newDeg, g4))

randomArgumentIO :: RandomArgumentSettings -> IO Argument
randomArgumentIO setts = SR.getStdRandom (randomArgument setts)

    
-- SECTION: Random valuations

-- | Given a list of Atomic and a RandomGen,
-- | produces a Valuation defined on those Atomic and a new RandomGen.
-- | If the list is empty, it passes thru the old RandomGen.
randomValuationSelectAtomics :: (Random v, RandomGen g)
                             => [Atomic]         -- ^ the Atomics to include
                             -> g                -- ^ the current RandomGen
                             -> (Valuation v, g) -- ^ the Valuation and new RandomGen
randomValuationSelectAtomics ats g =
    foldl' (\(accmap, g1) at -> let (rval, g2) = SR.random g1
                                in (Map.insert at rval accmap, g2))
           (Map.empty, g)
           ats

-- | Given a list of Atomics,
-- produces a random Valuation on those Atomics
-- using and updating the global generator
randomValuationSelectAtomicsIO :: (Random v)
                               => [Atomic]
                               -> IO (Valuation v)
randomValuationSelectAtomicsIO ats = SR.getStdRandom (randomValuationSelectAtomics ats)
