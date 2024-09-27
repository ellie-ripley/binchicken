{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Random where

import Data.Bifunctor (first)
import Data.List (foldl', nub, (\\))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack, unpack)

import Logic.Formulas
import Logic.Arguments
import Logic.Valuations
import Logic.IntValid
import Logic.Lambdas.Types
import Logic.Lambda

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
-- | This ignores the degree weighting in the random formula settings, except to determine
-- | a maximum degree
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


-- SECTION: Random intuitionistically valid arguments

randomElement
  :: RandomGen g
  => [a]
  -> g
  -> (a, g)
randomElement xs g =
  let (ix, g1) = SR.randomR (0, length xs - 1) g
  in  (xs !! ix, g1)

randomElementIO :: [a] -> IO a
randomElementIO xs = SR.getStdRandom $ randomElement xs

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = \case
                        Nothing -> Left x
                        Just y  -> Right y


-- | gives a random DId or FE
-- TODO: weightings are currently hardcoded here, and throughout other argument-building functions
-- break this out into a "Settings" argument?
randomAxiom
  :: RandomGen g
  => RandomFormulaSettings
  -> g
  -> (Argument, g)
randomAxiom setts g =
  let (numPrems, g1) = SR.randomR (1, 2) g
      (coin, g2) = SR.randomR (0 :: Int, 99) g1
      threshold = 95 -- percent chance this is a DId rather than FE
      (prems, g3) = if coin < threshold
                    then randomFormulas setts numPrems g2
                    else let (coin2, h) = SR.random g2
                             (fm, h1) = randomFormula setts h
                         in if coin2
                            then ([N Falsum, fm], h1)
                            else ([fm, N Falsum], h1)
      (conc, g4) = if coin < threshold
                   then randomElement prems g3
                   else randomFormula setts g3
  in (Argument (nub prems) conc, g4)

-- | Flips a weighted coin; Int is percent chance to Remove
randomRemove
  :: RandomGen g
  => Int
  -> g
  -> (Remove, g)
randomRemove pct g =
  let (coin, h) = SR.randomR (0, 99) g
  in  if coin < pct
      then (Remove, h)
      else (Keep, h)


addRandomACL
  :: RandomGen g
  => Argument
  -> RandomFormulaSettings
  -> g
  -> Either Text (Argument, g)
addRandomACL arg@(Argument prems _) setts g
  | null prems = Left "Error: ACL with empty premises"
  | otherwise  = Right (result, h)
  where
    (side, g1) = SR.randomR (minBound, maxBound) g
    (remove, g2) = randomRemove 80 g1
    (activePrem, g3) = randomElement prems g2
    (otherConj, g4) = randomFormula setts g3
    activeConc = case side of
                   LeftSide -> conj activePrem otherConj
                   RightSide -> conj otherConj activePrem
    (result, h) = case additiveConjLeft arg activeConc side remove of
                   Nothing -> error "Error 1289: this is an error in the website; please let me know about it!" -- this line should never happen!
                   Just res -> (res, g4)

addRandomMCL
  :: RandomGen g
  => Argument
  -> g
  -> Either Text (Argument, g)
addRandomMCL arg@(Argument prems _) g
  | null prems = Left "Error: MCL with empty premises"
  | otherwise  = Right (result, h)
  where
    (lc, g1) = randomElement prems g
    (rc, g2) = randomElement prems g1
    (reml, g3) = randomRemove 80 g2
    (remr, g4) = randomRemove 80 g3
    (result, h) = case multiplicativeConjLeft arg (conj lc rc) reml remr of
                    Nothing -> error "Error 3098: this is an error in the website; please let me know about it!"
                    Just res -> (res, g4)

-- Don't need addMCR; just use multiplicativeConjRight directly; there's nothing to randomize

addRandomMDL
  :: RandomGen g
  => Argument
  -> Argument
  -> g
  -> Either Text (Argument, g)
addRandomMDL a1@(Argument ps1 _) a2@(Argument ps2 _) g
  | null ps1 || null ps2 = Left "Error: MDL with some empty premises"
  | otherwise = Right (result, h)
  where
    (ld, g1) = randomElement ps1 g
    (rd, g2) = randomElement ps2 g1
    (reml, g3) = randomRemove 80 g2
    (remr, g4) = randomRemove 80 g3
    (side, g5) = SR.randomR (minBound, maxBound) g4
    (result, h) = case multiplicativeDisjLeft a1 a2 (disj ld rd) reml remr side of
                   Nothing -> error "Error 0987: error in the website; please let me know!"
                   Just res -> (res, g5)

addRandomADR
  :: RandomGen g
  => Argument
  -> RandomFormulaSettings
  -> g
  -> (Argument, g)
addRandomADR arg setts g =
  let (side, g1) = SR.randomR (minBound, maxBound) g
      (od, g2) = randomFormula setts g1
  in  (additiveDisjRight arg side od, g2)

addRandomMIL
  :: RandomGen g
  => Argument
  -> Argument
  -> g
  -> Either Text (Argument, g)
addRandomMIL a1 a2@(Argument ps2 _) g
  | null ps2 = Left "Error: MIL with empty premises"
  | otherwise = let (cons, g1) = randomElement ps2 g
                    mres = multiplicativeImplLeft a1 a2 cons
                in case mres of
                     Nothing -> Left "Error: MIL failed"
                     Just res -> Right (res, g1)

addRandomMIR
  :: RandomGen g
  => Argument
  -> RandomFormulaSettings
  -> g
  -> (Argument, g)
addRandomMIR arg@(Argument prems _) setts g
  | null prems = let (ante, g1) = randomFormula setts g
                 in (multiplicativeImplRight arg ante Remove, g1)
  | otherwise  = let (coin, g1) = SR.randomR (0, 99) g
                     (ante, g2) = if coin < (80 :: Int)
                                  then randomElement prems g1
                                  else randomFormula setts g1
                     (remove, g3) = randomRemove 85 g2
                in (multiplicativeImplRight arg ante remove, g3)

addRandomNR
  :: RandomGen g
  => Argument
  -> g
  -> Either Text (Argument, g)
addRandomNR arg@(Argument prems conc) g
  | null prems = Left "Error: NR with empty premises"
  | not $ conc == fum = Left "Error: NR with non-falsum conclusion"
  | otherwise = let (fm, g1) = randomElement prems g
                    (remove, g2) = randomRemove 80 g1
                    mres = negationRight arg fm remove
                in case mres of
                     Nothing -> Left "Error: bad NR"
                     Just res -> Right (res, g2)

addRandomUnaryRule
  :: RandomGen g
  => Argument
  -> RandomFormulaSettings
  -> g
  -> Either Text (Argument, g)
addRandomUnaryRule arg@(Argument prems conc) setts g =
  let dropNR = if conc == fum then [] else [NR]
      dropCL = if null prems then [ACL, MCL] else []
      rulesToDrop = dropNR <> dropCL
      rulesToChooseFrom = sequentRulesWithArity 1 \\ rulesToDrop
      (rl, g1) = randomElement rulesToChooseFrom g
  in case rl of
       ACL -> addRandomACL arg setts g1
       MCL -> addRandomMCL arg g1
       ADR -> Right $ addRandomADR arg setts g1
       MIR -> Right $ addRandomMIR arg setts g1
       NL -> Right $ (negationLeft arg, g1)
       NR -> addRandomNR arg g1
       x   -> Left ("Error: A rule with the wrong arity (not 1) snuck in! " <> (pack $ show x))

addRandomBinaryRule
  :: RandomGen g
  => Argument
  -> Argument
  -> g
  -> Either Text (Argument, g)
addRandomBinaryRule a1@(Argument ps1 _) a2@(Argument ps2 _) g =
  let dropMIL = if null ps2 then [MIL] else []
      dropMDL = if null ps1 || null ps2 then [MDL] else []
      rulesToDrop = dropMIL <> dropMDL
      rulesToChooseFrom = sequentRulesWithArity 2 \\ rulesToDrop
      (rl, g1) = randomElement rulesToChooseFrom g
  in case rl of
       MCR -> Right $ (multiplicativeConjRight a1 a2, g1)
       MDL -> addRandomMDL a1 a2 g1
       MIL -> addRandomMIL a1 a2 g1
       x   -> Left ("A rule with the wrong arity (not 2) snuck in! " <> (pack $ show x))


randomIntValidArgument
  :: RandomGen g
  => RandomFormulaSettings
  -> Int -- ^ max rule depth
  -> g
  -> (Argument, g)
randomIntValidArgument setts 0 g = randomAxiom setts g
randomIntValidArgument setts maxDepth g
  | maxDepth < 0 = error "Can't make a proof with negative size; please report this error!"
  | otherwise = let (coin, g1) = SR.randomR (0 :: Int, 99) g
                in if coin > 95 -- chance to keep going at each step
                   then randomAxiom setts g1
                   else let (arity, g2) = SR.randomR (1 :: Int, 2) g1
                        in case arity of
                            1 ->
                              let (a1, g3) = randomIntValidArgument setts (maxDepth - 1) g2
                                  mres = addRandomUnaryRule a1 setts g3
                              in case mres of
                                Left err -> error ("Problem in the recursion alas: " <> unpack err)
                                Right res -> res
                            2 ->
                              let (a1, g3) = randomIntValidArgument setts (maxDepth - 1) g2
                                  (a2, g4) = randomIntValidArgument setts (maxDepth - 1) g3
                                  mres = addRandomBinaryRule a1 a2 g4
                              in case mres of
                                Left err -> error ("Problem in the other recursion: " <> unpack err)
                                Right res -> res
                            _ -> error "liuhfdsljn"

randomIntValidArgumentIO :: RandomFormulaSettings -> IO Argument
randomIntValidArgumentIO setts = SR.getStdRandom $ randomIntValidArgument setts 2


-- SECTION: Lambda calculus terms



randomLambdaTermFixedComplexity
  :: RandomGen g
  => [LVar]    -- ^ variables to use
  -> Int       -- ^ weight (number of Apps + Lams)
  -> g
  -> (Term, g)
randomLambdaTermFixedComplexity vrs 0 g =
  let (lvr, g1) = randomElement vrs g in (TVar lvr, g1)
randomLambdaTermFixedComplexity vrs w g =
  let (coin, g1) = SR.randomR (1 :: Int, 2) g
  in case coin of
    1 -> let (leftWeight, g2) = SR.randomR (0, w) g1
             (left, g3)  = randomLambdaTermFixedComplexity vrs leftWeight g2
             (right, g4) = randomLambdaTermFixedComplexity vrs (w - leftWeight) g3
         in (TApp left right, g4)
    2 -> let (body, g2) = randomLambdaTermFixedComplexity vrs (w - 1) g1
             fvs = freeVars body
             numFVs = length fvs
             (newVarIx, g3) = SR.randomR (0, numFVs) g2
             (newVar, g4) = case newVarIx of
                              numFVs -> randomElement vrs g3
                              n      -> (fvs !! n, g3)
         in (TLam newVar body, g4)

randomLambdaTerm
  :: RandomGen g
  => [LVar]    -- ^ variables to use
  -> Int       -- ^ maximum weight (Apps + Lams)
  -> g
  -> (Term, g)
randomLambdaTerm vrs w g =
  let (weight, g1) = SR.randomR (0, w) g in randomLambdaTermFixedComplexity vrs weight g1
