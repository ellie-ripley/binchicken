{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Sequents where

import Data.Bifunctor (first)
import qualified Data.Text as T
import System.Random (RandomGen)
import qualified System.Random as SR

import Logic.Arguments ( Argument(..) )
import Logic.Formulas ( degree
                      , conj
                      , disj
                      , impl
                      , fum
                      , neg, displayFormula
                      )
import Logic.Random ( randomArgument
                    , randomFormula
                    , randomFormulas
                    )
import Settings.Binchicken ( RandomSequentPreProofSettings(..)
                           , setRuleNum
                           , RandomArgumentSettings(..)
                           , RandomFormulaSettings(..)
                           )
import Logic.PreProofs (RawProofTree(..))

data SequentNullaryRule
  = ID
  | FL
  deriving (Eq, Show, Enum, Ord, Bounded)

data SequentUnaryRule
  = CL
  | DRL
  | DRR
  | NL
  | NR
  | IR
  deriving (Eq, Show, Enum, Ord, Bounded)

data SequentBinaryRule
  = CR
  | DL
  | IL
  deriving (Eq, Show, Enum, Ord, Bounded)

data SequentRule
  = SRN SequentNullaryRule
  | SRU SequentUnaryRule
  | SRB SequentBinaryRule
  deriving (Eq, Show)

instance Enum SequentRule where
  toEnum i
      | i < unOffset  = SRN (toEnum i)
      | i < biOffset  = SRU (toEnum (i - unOffset))
      | i < triOffset = SRB (toEnum (i - biOffset))
      | otherwise = error "Not that many sequent rules!"
      where
        unOffset  = fromEnum (maxBound :: SequentNullaryRule) + 1
        biOffset  = fromEnum (maxBound :: SequentUnaryRule) + unOffset + 1
        triOffset = fromEnum (maxBound :: SequentBinaryRule) + biOffset + 1

  fromEnum = \case
      (SRN n) -> fromEnum n
      (SRU u) -> fromEnum u + unOffset
      (SRB b) -> fromEnum b + biOffset
      where
          unOffset = fromEnum (maxBound :: SequentNullaryRule) + 1
          biOffset = fromEnum (maxBound :: SequentUnaryRule) + unOffset + 1

instance Bounded SequentRule where
  minBound = toEnum 0
  maxBound = toEnum maxI
      where
          maxI = fromEnum (maxBound :: SequentNullaryRule)
               + fromEnum (maxBound :: SequentUnaryRule)
               + fromEnum (maxBound :: SequentBinaryRule)
               + 2

data SequentPreProof a
  = SPPN SequentNullaryRule a
  | SPPU SequentUnaryRule (SequentPreProof a) a
  | SPPB SequentBinaryRule (SequentPreProof a) (SequentPreProof a) a
  deriving (Eq, Show)


-- | Gives a random preproof skeleton (just rules, no actual sequents)
-- | Initial sequent weights are hardcoded here
-- | No guarantee the skeleton can be correctly filled
randomSPPSkeleton
  :: (RandomGen g)
  => RandomSequentPreProofSettings
  -> g
  -> (SequentPreProof (), g)
randomSPPSkeleton setts g
  | rsppMaxRules setts < 0 = error "Can't have negatively many rules!"
  | rsppMinRules setts > rsppMinRules setts = error "Minimum rules too high!"
  | otherwise =
      let minR = rsppMinRules setts
          maxR = rsppMaxRules setts
          -- minR and maxR will match on recursive calls; if so skip the roll
          (numR, g1) = if minR == maxR
                       then (minR, g)
                       else SR.randomR (minR, maxR) g
      in case numR of
           -- 0 rules: initial sequent
           0 -> let (rl, g2) = SR.randomR (0, 8 :: Int) g1
                in if rl == 0
                   then (SPPN FL (), g2)
                   else (SPPN ID (), g2)
           -- d many rules: pick a rule, divide if needed, and recurse
           r -> let rulesToChooseFrom = map SRU [minBound..maxBound :: SequentUnaryRule]
                                        ++ map SRB [minBound..maxBound :: SequentBinaryRule]
                    (ix, g2) = SR.randomR (0, length rulesToChooseFrom - 1) g1
                in case rulesToChooseFrom !! ix of
                     (SRN _) -> error "I promise this won't happen"
                     (SRU u) -> let newSetts = setRuleNum (r - 1) setts
                                    (component, g3) = randomSPPSkeleton newSetts g2
                                in (SPPU u component (), g3)
                     (SRB b) -> let (r1, g3) = SR.randomR (0, r - 1) g2
                                    setts1 = setRuleNum r1 setts
                                    setts2 = setRuleNum ((r - 1) - r1) setts
                                    (comp1, g4) = randomSPPSkeleton setts1 g3
                                    (comp2, g5) = randomSPPSkeleton setts2 g4
                                in (SPPB b comp1 comp2 (), g5)

concSequent :: SequentPreProof Argument -> Argument
concSequent = \case
  SPPN _ ar -> ar
  SPPU _ _ ar -> ar
  SPPB _ _ _ ar -> ar

-- | can have one premise more than setts allows, in some FL cases
fillNullary
  :: (RandomGen g)
  => RandomArgumentSettings
  -> g
  -> SequentNullaryRule
  -> (SequentPreProof Argument, g)
fillNullary setts g rl = first (SPPN rl) argPair
  where
    newMin = max 0 $ raMinPremises setts - 1
    midMax = max 0 $ raMaxPremises setts - 1
    newMax = max newMin midMax
    newSetts = setts { raMinPremises = newMin, raMaxPremises = newMax }
    (Argument prems conc, g1) = randomArgument newSetts g
    argPair
      = case rl of
          ID -> (Argument (conc : prems) conc, g1)
          FL -> (Argument (fum  : prems) conc, g1)

-- | can make formulas bigger than setts allows, but tries to minimize this in places
-- | hardcodes some odds
-- TODO: Fix NR case, which just bombs out for now
fillUnary
  :: (RandomGen g)
  => RandomFormulaSettings
  -> g
  -> SequentUnaryRule
  -> SequentPreProof Argument
  -> (Maybe (SequentPreProof Argument), g)
fillUnary setts g rl compon = first (SPPU rl compon <$>) mArgPair
  where
    (Argument oldPrems oldConc) = concSequent compon
    used = degree oldConc
    oldWeights = rfDegreeWeights setts
    newMax = max 0 $ ((length oldWeights) - used) - 2
    newWeights = take (newMax + 1) oldWeights
    newSetts = setts { rfDegreeWeights = newWeights }
    mArgPair = case rl of
      CL ->
        let (midPrems, g1)
              | length oldPrems < 2 =
                  let (kForms, h) = randomFormulas setts (2 - length oldPrems) g
                  in  (kForms ++ oldPrems, h)
              | otherwise = (oldPrems, g)
            (ix1, g2) = SR.randomR (0, length midPrems - 1) g1
            (ix2, g3) = SR.randomR (0, length midPrems - 1) g2
            (leftConj, rightConj) = (midPrems !! ix1, midPrems !! ix2)
            newConj = conj leftConj rightConj
            (ix3, ix4) = (min ix1 ix2, max ix1 ix2)
            chunk1 = take ix3 midPrems
            chunk2 = drop (ix3 + 1) (take ix4 midPrems)
            chunk3 = drop (ix4 + 1) midPrems
            restOfPrems = chunk1 ++ chunk2 ++ chunk3
        in  (Just $ Argument (newConj : restOfPrems) oldConc, g3)
      DRL ->
        let (newDisj, g1) = randomFormula newSetts g
            newConc = disj newDisj oldConc
        in  (Just $ Argument oldPrems newConc, g1)
      DRR ->
        let (newDisj, g1) = randomFormula newSetts g
            newConc = disj oldConc newDisj
        in  (Just $ Argument oldPrems newConc, g1)
      NL -> (Just $ Argument (neg oldConc : oldPrems) fum, g)
      NR -> (Nothing, g)
      IR
        | null oldPrems || vacuous ->
            let (newAnt, g2) = randomFormula newSetts g
                newConc = impl newAnt oldConc
            in  (Just $ Argument oldPrems newConc, g2)
        | otherwise ->
            let (ix, g2) = SR.randomR (0, length oldPrems - 1) g1
                newConc  = impl (oldPrems !! ix) oldConc
                newPrems = take ix oldPrems ++ drop (ix + 1) oldPrems
            in  (Just $ Argument newPrems newConc, g2)
        where
          (roll, g1) = SR.randomR (0, 9 :: Int) g
          vacuous    = roll == 0

fillBinary
  :: (RandomGen g)
  => RandomFormulaSettings
  -> g
  -> SequentBinaryRule
  -> SequentPreProof Argument
  -> SequentPreProof Argument
  -> (Maybe (SequentPreProof Argument), g)
fillBinary setts g rl comp1 comp2 = first (SPPB rl comp1 comp2 <$>) argPair
  where
    (Argument prems1 conc1) = concSequent comp1
    (Argument prems2 conc2) = concSequent comp2
    argPair = case rl of
      CR -> (Just $ Argument (prems1 ++ prems2) (conj conc1 conc2), g)
      DL ->
        if conc1 /= conc2
        then (Nothing, g)
        else
          let (midPrems1, g1)
                | null prems1 = first (:[]) $ randomFormula setts g
                | otherwise   = (prems1, g)
              (midPrems2, g2)
                | null prems2 = first (:[]) $ randomFormula setts g1
                | otherwise   = (prems2, g1)
              (ix1, g3) = SR.randomR (0, length midPrems1 - 1) g2
              (ix2, g4) = SR.randomR (0, length midPrems2 - 1) g3
              leftD  = midPrems1 !! ix1
              rightD = midPrems2 !! ix2
              newPrems1 = take ix1 midPrems1 ++ drop (ix1 + 1) midPrems1
              newPrems2 = take ix2 midPrems2 ++ drop (ix2 + 1) midPrems2
              newDisj = disj leftD rightD
          in  (Just $ Argument (newDisj : newPrems1 ++ newPrems2) conc1, g4)
      IL ->
        let (midPrems2, g1)
              | null prems2 = first (:[]) $ randomFormula setts g
              | otherwise   = (prems2, g)
            (ix2, g2) = SR.randomR (0, length midPrems2 - 1) g1
            newImp = impl conc1 (midPrems2 !! ix2)
            newPrems2 = take ix2 midPrems2 ++ drop (ix2 + 1) midPrems2
        in  (Just $ Argument (newImp : prems1 ++ newPrems2) conc2, g2)

fillSkeleton
  :: (RandomGen g)
  => RandomArgumentSettings
  -> g
  -> SequentPreProof ()
  -> (Maybe (SequentPreProof Argument), g)
fillSkeleton setts g = \case
  SPPN snr _ -> first Just $ fillNullary setts g snr
  SPPU sur comp _ ->
    let formSetts   = rarfSettings setts
        (mComp, g1) = fillSkeleton setts g comp
    in case mComp of
         Nothing   -> (Nothing, g1)
         Just com -> fillUnary formSetts g1 sur com
  SPPB sbr comp1 comp2 _ ->
    let formSetts  = rarfSettings setts
        (mComp1, g1) = fillSkeleton setts g comp1
        (mComp2, g2) = fillSkeleton setts g1 comp2
    in case (mComp1, mComp2) of
          (Nothing, _) -> (Nothing, g2)
          (_, Nothing) -> (Nothing, g2)
          (Just com1, Just com2) -> fillBinary formSetts g2 sbr com1 com2

randomMaybeSequentProof
  :: (RandomGen g)
  => RandomSequentPreProofSettings
  -> g
  -> (Maybe (SequentPreProof Argument), g)
randomMaybeSequentProof ppSetts g
  = let (skel, g1) = randomSPPSkeleton ppSetts g
        argSetts = rsppraSettings ppSetts
    in fillSkeleton argSetts g1 skel

randomSequentProofIO :: RandomSequentPreProofSettings -> IO (SequentPreProof Argument)
randomSequentProofIO ppSetts = do
  mSpp <- SR.getStdRandom $ randomMaybeSequentProof ppSetts
  case mSpp of
    Nothing  -> randomSequentProofIO ppSetts
    Just spp -> return spp

randomIntValidArgFilterIO :: (Argument -> Bool) -> RandomSequentPreProofSettings -> IO Argument
randomIntValidArgFilterIO prd ppSetts = do
  arg <- concSequent <$> randomSequentProofIO ppSetts
  if prd arg
    then return arg
    else randomIntValidArgFilterIO prd ppSetts




-- Render functions used to make proofJS display a SequentPreProof; useful for debugging but unused in site

renderArg :: Argument -> T.Text
renderArg (Argument prems conc) =
  T.intercalate ", " (map displayFormula prems)
    <> " / "
    <> displayFormula conc

renderSequentProof :: SequentPreProof Argument -> RawProofTree
renderSequentProof = \case
  (SPPN n arg) ->
    RawProofTree
      { label = renderArg arg
      , rule  = T.pack $ show n
      , forest = [RawProofTree { label = "", rule = "", forest = [] }]
      }
  (SPPU u comp arg) ->
    RawProofTree
      { label = renderArg arg
      , rule = T.pack $ show u
      , forest = [renderSequentProof comp]
      }
  (SPPB b comp1 comp2 arg) ->
    RawProofTree
      { label = renderArg arg
      , rule  = T.pack $ show b
      , forest = [renderSequentProof comp2, renderSequentProof comp1]
      }
