{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Logic.Proofs where

import Import ( shamlet
              , whamlet
              , Widget
              )

import Data.Aeson (ToJSON, FromJSON)
import Data.List ((\\), intersect, maximumBy, nub)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Logic.Arguments
  (Argument(..))
import Logic.Formulas
  ( Formula
  , GenFormula(..)
  , NullaryConnective(..)
  , UnaryConnective(..)
  , BinaryConnective(..)
  , Connective(..)
  , degree
  , displayFormula
  , displayConnective
  )
import Logic.PreProofs
    ( PreProofParseError
    , PreProof(..)
    , TrinaryRule(..)
    , BinaryRule(..)
    , UnaryRule(..)
    , Rule(..)
    , ppConclusion
    , ppOpenAssumptions
    , ppDischargedAssumptions
    , ppRulesIn
    , ppSize
    , ruleName
    )

data ProofStatus
  = GoodProof
  | BadPreProof PreProofParseError
  | MissingDischarge Text Formula
  | DuplicateDischarge Text
  | BadlyFormedAtRule Rule
  | BadlyFormedDischarge Rule Formula Formula --badformula goodformula
  deriving (Show, Generic, ToJSON, FromJSON)

instance Semigroup ProofStatus where
  (<>) GoodProof = id
  (<>) ps = const ps

displayProofStatus :: ProofStatus -> String
displayProofStatus ps = renderHtml disp
  where disp = case ps of
          GoodProof -> [shamlet|<p>Good proof!|]
          BadPreProof _ -> [shamlet|<p>Bad preproof!|]
          MissingDischarge tx fm ->
            [shamlet|<p>The formula <code>#{displayFormula fm}</code> is discharged with label <code>#{tx}</code>, but no rule allows for this!|]
          DuplicateDischarge tx ->
            [shamlet|<p>Use different discharge labels for different rules! You've used the label <code>#{tx}</code> more than once.|]
          BadlyFormedAtRule rl -> [shamlet|<p>A step of <code>#{ruleName rl}</code> is badly formed!|]
          BadlyFormedDischarge rl badfm goodfm ->
            [shamlet|<p>A step of <code>#{ruleName rl}</code> discharges <code>#{displayFormula badfm}</code> in a place where it's only allowed to discharge <code>#{displayFormula goodfm}</code>!|]



checkProof :: PreProof -> ProofStatus
checkProof = checkProofInContext []

lookForDischarge :: (Text, Formula) -> [(Text, Rule, Formula)] -> ProofStatus
lookForDischarge (lb, fm) [] = MissingDischarge lb fm
lookForDischarge (lb, fm) ((clb, crl, cfm):cs)
  | lb == clb && fm == cfm = GoodProof
  | lb == clb = BadlyFormedDischarge crl fm cfm
  | otherwise = lookForDischarge (lb, fm) cs



-- | This is where the proof system really lives!
-- | a context is a list of (discharge label, rule it came from, formula discharged)
-- | all the checking for context correctness is done when a member of the list is added
checkProofInContext :: [(Text, Rule, Formula)] -> PreProof -> ProofStatus
checkProofInContext _ (Open _) = GoodProof
checkProofInContext con (Discharged lb fm) = lookForDischarge (lb, fm) con
checkProofInContext con (UR CEL pp1 q) =
  case (ppConclusion pp1) of
    (B Conjunction r _) -> if q == r
                           then checkProofInContext con pp1
                           else BadlyFormedAtRule (RU CEL)
    _                   -> BadlyFormedAtRule (RU CEL)
checkProofInContext con (UR CER pp1 q) =
  case (ppConclusion pp1) of
    (B Conjunction _ r) -> if q == r
                           then checkProofInContext con pp1
                           else BadlyFormedAtRule (RU CER)
    _                   -> BadlyFormedAtRule (RU CER)
checkProofInContext con (UR DIL pp1 q) =
  case q of
    (B Disjunction r _) -> if ppConclusion pp1 == r
                           then checkProofInContext con pp1
                           else BadlyFormedAtRule (RU DIL)
    _                   -> BadlyFormedAtRule (RU DIL)
checkProofInContext con (UR DIR pp1 q) =
  case q of
    (B Disjunction _ r) -> if ppConclusion pp1 == r
                           then checkProofInContext con pp1
                           else BadlyFormedAtRule (RU DIR)
    _                   -> BadlyFormedAtRule (RU DIR)
checkProofInContext con (UR (II lb) pp1 q) =
  case q of
    (B Implication r s) -> if lb `elem` map (\(a, _, _) -> a) con
                                then DuplicateDischarge lb
                           else if ppConclusion pp1 == s
                                then checkProofInContext ((lb, RU $ II lb, r) : con) pp1
                           else BadlyFormedAtRule (RU $ II lb)
    _                   -> BadlyFormedAtRule (RU $ II lb)
checkProofInContext con (UR (NI lb) pp1 q) =
  case q of
    (U Negation r) -> if lb `elem` map (\(a, _, _) -> a) con
                           then DuplicateDischarge lb
                      else if ppConclusion pp1 == N Falsum
                           then checkProofInContext ((lb, RU $ NI lb, r) : con) pp1
                      else BadlyFormedAtRule (RU $ NI lb)
    _              -> BadlyFormedAtRule (RU $ NI lb)
checkProofInContext con (UR FE pp1 _) =
  case (ppConclusion pp1) of
    (N Falsum) -> checkProofInContext con pp1
    _          -> BadlyFormedAtRule (RU FE)
checkProofInContext con (BR CI pp1 pp2 q) =
  case q of
    (B Conjunction r s) -> if ppConclusion pp1 == s && ppConclusion pp2 == r
                           then checkProofInContext con pp1 <> checkProofInContext con pp2
                           else BadlyFormedAtRule (RB CI)
    _                   -> BadlyFormedAtRule (RB CI)
checkProofInContext con (BR IE pp1 pp2 q) =
  case ppConclusion pp2 of
    (B Implication r s) ->
      if ppConclusion pp1 == r && s == q
      then checkProofInContext con pp1 <> checkProofInContext con pp2
      else BadlyFormedAtRule (RB IE)
    _ -> BadlyFormedAtRule (RB IE)
checkProofInContext con (BR NE pp1 pp2 q) =
  case ppConclusion pp2 of
    (U Negation r) ->
      if ppConclusion pp1 == r && q == N Falsum
      then checkProofInContext con pp1 <> checkProofInContext con pp2
      else BadlyFormedAtRule (RB NE)
    _ -> BadlyFormedAtRule (RB NE)
checkProofInContext con (TR (DE lb) pp1 pp2 pp3 q) =
  case ppConclusion pp3 of
    (B Disjunction r s) ->
      if ppConclusion pp1 == ppConclusion pp2 && ppConclusion pp1 == q
        then if lb `elem` map (\(a, _, _) -> a) con
                then DuplicateDischarge lb
             else checkProofInContext ((lb, RT $ DE lb,  s) : con) pp1
                  <> checkProofInContext ((lb, RT $ DE lb, r) : con) pp2
                  <> checkProofInContext con pp3
      else BadlyFormedAtRule (RT $ DE lb)
    _ -> BadlyFormedAtRule (RT $ DE lb)

-- Requirements

data ProofRequirements =
  ProofRequirements { reqConclusion :: Formula
                    , reqOpenAssumptions :: [Formula]
                    , reqDischarged :: [Formula]
                    , reqMinRules :: Int
                    , reqUsedRules :: [Rule]
                    , reqUnusedRules :: [Rule]
                    } deriving (Show, Generic, FromJSON, ToJSON)

data CheckRequirements
  = MeetsRequirements
  | WrongConclusion Formula Formula -- bad then good
  | TooSmall Int
  | MissingOpenAssumption Formula
  | MissingDischarged Formula
  | MissingRule Rule
  | HasBannedRule Rule
  | BadProof ProofStatus
  deriving (Show, Generic, ToJSON, FromJSON)

checkRequirements :: ProofRequirements -> PreProof -> CheckRequirements
checkRequirements reqs pp = case checkProof pp of
  GoodProof
    | ppConclusion pp /= reqConclusion reqs -> WrongConclusion (ppConclusion pp) (reqConclusion reqs)
    | ppSize pp < reqMinRules reqs -> TooSmall $ ppSize pp
    | (f:_) <- nub (reqOpenAssumptions reqs) \\ ppOpenAssumptions pp -> MissingOpenAssumption f
    | (f:_) <- nub (reqDischarged reqs) \\ ppDischargedAssumptions pp -> MissingDischarged f
    | (f:_) <- nub (reqUsedRules reqs) \\ ppRulesIn pp -> MissingRule f
    | (f:_) <- (nub $ reqUnusedRules reqs) `intersect` ppRulesIn pp -> HasBannedRule f
    | otherwise -> MeetsRequirements
  bad -> BadProof bad

displayRequirementsCheck :: CheckRequirements -> String
displayRequirementsCheck cr = renderHtml disp
  where disp = case cr of
                MeetsRequirements ->
                  [shamlet|<p>This is a correct proof, and it meets the requirements. Nice going!|]
                WrongConclusion badfm goodfm ->
                  [shamlet|<p>Missed requirement: This proof has conclusion <code>#{displayFormula badfm}</code>, but the requirements say it needs the conclusion <code>#{displayFormula goodfm}</code>!|]
                TooSmall sz ->
                  [shamlet|<p>Missed requirement: This proof uses just #{show sz} rules; it's too small!|]
                MissingOpenAssumption fm ->
                  [shamlet|<p>Missed requirement: This proof does not have <code>#{displayFormula fm}</code> among its open assumptions!|]
                MissingDischarged fm ->
                  [shamlet|<p>Missed requirement: This proof does not have <code>#{displayFormula fm}</code> among its discharged assumptions!|]
                MissingRule r ->
                  [shamlet|<p>Missed requirement: This proof does not use the rule <code>#{ruleName r}</code>!|]
                HasBannedRule r ->
                  [shamlet|<p>Missed requirement: This proof uses the rule <code>#{ruleName r}</code>!|]
                BadProof ps -> preEscapedToHtml $ displayProofStatus ps

-- Proof of arguments

data CheckProofOf
  = ProofOfArgument Argument
  | BadProofOf ProofStatus
  | WrongConclusionOf Formula Formula -- bad then required
  | ExtraAssumption Formula
  deriving (Show, Generic, ToJSON, FromJSON)


checkProofOf :: Argument -> PreProof -> CheckProofOf
checkProofOf arg@(Argument prems conc) pp = case checkProof pp of
  GoodProof
    | ppConclusion pp /= conc -> WrongConclusionOf (ppConclusion pp) conc
    | (f:_) <- (nub $ ppOpenAssumptions pp) \\ prems -> ExtraAssumption f
    | otherwise -> ProofOfArgument arg
  bad -> BadProofOf bad

displayCheckProofOf :: CheckProofOf -> String
displayCheckProofOf cpo = renderHtml disp
  where disp = case cpo of
                 ProofOfArgument _ ->
                   [shamlet|<p>This is a correct proof of the argument. Nice one!|]
                 WrongConclusionOf badfm goodfm ->
                   [shamlet|<p>This proof has conclusion <code>#{displayFormula badfm}</code>, but it would need the conclusion <code>#{displayFormula goodfm}</code> to be a proof of this argument!|]
                 ExtraAssumption fm ->
                   [shamlet|<p>This proof has <code>#{displayFormula fm}</code> as an open assumption, but that's not one of the argument's premises!|]
                 BadProofOf ps -> preEscapedToHtml $ displayProofStatus ps

-- Normalization

data CheckNormalized
  = CorrectAndNormalized
  | OriginalMissingRequiredSegment Connective
  | OriginalNoSegmentLongEnough Connective Int Int -- longest segment present vs required length
  | OriginalBadProof ProofStatus
  | NormalizedBadProof ProofStatus
  | NormalizedHasExtraAssumption Formula
  | NormalizedHasMaxSegment
  | NormalizedHasNoncomplyingFE
  | NormalizedWrongNormalForm
  deriving (Show, Generic, ToJSON, FromJSON)

data PNRequirements = PNRequirements
  { pnConclusion :: Formula
  , pnMainConnective :: Connective
  , pnSegmentLength :: Int
  } deriving (Generic, ToJSON, FromJSON)

renderPNRequirements :: PNRequirements -> Widget
renderPNRequirements (PNRequirements pnConc pnMC pnSL)
  = [whamlet|<p>Your original proof should:
             <ul>
                <li>Have the conclusion <code>#{displayFormula pnConc}</code>
                <li>Include at least one maximum segment with the main connective <code>#{displayConnective pnMC}</code> and length at least #{pack (show pnSL)}
    |]

displayCheckNormalized :: CheckNormalized -> String
displayCheckNormalized cn = renderHtml disp
  where disp = case cn of
          CorrectAndNormalized ->
            [shamlet|<p>Yes! These proofs meet the given requirements. Nice going!|]
          OriginalMissingRequiredSegment conn ->
            [shamlet|<p>Your starting proof doesn't have a maximum segment with the main connective #{displayConnective conn}!|]
          OriginalNoSegmentLongEnough conn badn goodn ->
            [shamlet|<p>Your starting proof's longest maximum segment with the main connective <code>#{displayConnective conn}</code> is only #{show badn} steps long; the requirements asked for one at least #{show goodn} steps long!|]
          OriginalBadProof bad -> [shamlet|<p>Your original proof is not a correct proof: |] <> preEscapedToHtml (displayProofStatus bad)
          NormalizedBadProof bad -> [shamlet|<p>Your normalized proof is not a correct proof: |] <> preEscapedToHtml (displayProofStatus bad)
          NormalizedHasExtraAssumption fm -> [shamlet|<p>Your normalized proof has an open assumption that's not in the original proof: <code>#{displayFormula fm}</code>|]
          NormalizedHasMaxSegment -> [shamlet|<p>Your normalized proof is not actually in normal form; it has at least one maximum segment in it!|]
          NormalizedHasNoncomplyingFE -> [shamlet|<p>Your normalized proof is not actually in normal form; it has a use of <code>#{ruleName (RU FE)}</code> with a compound conclusion!|]
          NormalizedWrongNormalForm -> [shamlet|<p>Your normalized proof is indeed in normal form, but it is not the normal form of the first proof!|]

-- TODO: implement actual checking for normalization
checkNormalized
  :: PNRequirements
  -> PreProof
  -> PreProof
  -> CheckNormalized
checkNormalized pnr opp npp = case (checkProof opp, checkProof npp) of
  (GoodProof, GoodProof) ->
    let conn = pnMainConnective pnr
    in case longestSegment (maxSegments opp) conn of
      Nothing -> OriginalMissingRequiredSegment conn
      Just (_, _, l)
        | l < pnSegmentLength pnr ->
            OriginalNoSegmentLongEnough conn l (pnSegmentLength pnr)
        | maxSegments npp /= [] ->
            NormalizedHasMaxSegment
        | noncomplyingFEs npp /= [] ->
            NormalizedHasNoncomplyingFE
        | (not . null) (ppOpenAssumptions npp \\ ppOpenAssumptions opp) ->
            NormalizedHasExtraAssumption (head $ ppOpenAssumptions npp \\ ppOpenAssumptions opp)
        | otherwise -> CorrectAndNormalized
  (GoodProof, bad) -> NormalizedBadProof bad
  (bad, _) -> OriginalBadProof bad

-- | Given a list of maximum segments, return longest segment in the list with the required connective, if any
longestSegment
  :: [(Connective, Int, Int)]
  -> Connective
  -> Maybe (Connective, Int, Int)
longestSegment segs conn =
  let connSegs = filter (\(c, _, _) -> c == conn) segs
  in if null connSegs
        then Nothing
        else Just $ maximumBy (\(_, _, l) (_, _, m) -> compare l m) connSegs


-- | given a preproof, assumed to be a proof,
-- | returns a list of all maximum segments contained in the proof,
-- | each with its main connective, degree, and length
maxSegments
  :: PreProof
  -> [(Connective, Int, Int)]
maxSegments = \case
  Open _ -> []
  Discharged _ _ -> []
  UR rl compon _ ->
    case rl of
      CEL -> case openSegments compon of
               Just (CB Conjunction, deg, lengs) ->
                 (map (\l -> (CB Conjunction, deg, l)) lengs)
                 ++ maxSegments compon
               _ -> maxSegments compon
      CER -> case openSegments compon of
               Just (CB Conjunction, deg, lengs) ->
                 (map (\l -> (CB Conjunction, deg, l)) lengs)
                 ++ maxSegments compon
               _ -> maxSegments compon
      _ -> maxSegments compon
  BR rl comp1 comp2 _ ->
    case rl of
      IE -> let oldSegments = maxSegments comp2 ++ maxSegments comp1
            in case openSegments comp2 of
                 Just (CB Implication, deg, lengs) ->
                   (map (\l -> (CB Implication, deg, l)) lengs)
                   ++ oldSegments
                 _ -> oldSegments
      NE -> let oldSegments = maxSegments comp2 ++ maxSegments comp1
            in case openSegments comp2 of
                 Just (CU Negation, deg, lengs) ->
                   (map (\l -> (CU Negation, deg, l)) lengs)
                   ++ oldSegments
                 _ -> oldSegments
      CI -> maxSegments comp2 ++ maxSegments comp1
  TR rl comp1 comp2 comp3 _ ->
    case rl of
      (DE _) -> let oldSegments = maxSegments comp3
                                  ++ maxSegments comp2
                                  ++ maxSegments comp1
                in case openSegments comp3 of
                     Just (CB Disjunction, deg, lengs) ->
                       (map (\l -> (CB Disjunction, deg, l)) lengs)
                       ++ oldSegments
                     _ -> oldSegments



-- | aux for maxSegments. Given a preproof, assumed to be a proof,
-- | checks to see whether the conclusion of this proof would be in a maximum segment if it were to go on to be immediately eliminated.
-- | If not, returns Nothing. If so, returns main connective, degree, and list of lengths of the maximum segments that would result
openSegments
  :: PreProof
  -> Maybe (Connective, Int, [Int])
openSegments = \case
  Open _ -> Nothing
  Discharged _ _ -> Nothing
  UR rl _ fm ->
    case rl of
      CEL -> Nothing
      CER -> Nothing
      DIL -> Just (CB Disjunction, degree fm, [1])
      DIR -> Just (CB Disjunction, degree fm, [1])
      (II _) -> Just (CB Implication, degree fm, [1])
      (NI _) -> Just (CU Negation, degree fm, [1])
      FE -> Nothing
  BR rl _ _ fm ->
    case rl of
      CI -> Just (CB Conjunction, degree fm, [1])
      IE -> Nothing
      NE -> Nothing
  TR rl comp1 comp2 _ _ ->
    case rl of
      (DE _) ->
        case (openSegments comp1, openSegments comp2) of
          (Nothing, Nothing) -> Nothing
          (Nothing, Just (c, d, ls)) -> Just (c, d, map (+1) ls)
          (Just (c, d, ls), Nothing) -> Just (c, d, map (+1) ls)
          (Just (c, d, ls), Just (_, _, ms)) ->
            Just (c, d, map (+1) (ls ++ ms))

renderMaxSegments
  :: [(Connective, Int, Int)]
  -> String
renderMaxSegments = \case
  [] -> renderHtml [shamlet|<p>No maximum segments in original proof!|]
  ss -> renderHtml
    [shamlet|
       <p>Maximum segments in original proof:
       <table>
          <thead>
              <tr>
                <th>Main connective
                <th>Degree
                <th>Length
          <tbody>
            $forall (c, d, l) <- ss
              <tr>
                <td><code>#{displayConnective c}</code>
                <td>#{pack (show d)}
                <td>#{pack (show l)}
    |]

-- | Given a preproof, assumed to be a proof, lists all compound conclusions of FE anywhere in the proof
noncomplyingFEs
  :: PreProof
  -> [Formula]
noncomplyingFEs = \case
  Open _ -> []
  Discharged _ _ -> []
  UR rl compon fm ->
    case rl of
      FE
        | degree fm > 0 -> fm : noncomplyingFEs compon
        | otherwise -> noncomplyingFEs compon
      _ -> noncomplyingFEs compon
  BR _ comp1 comp2 _ ->
    noncomplyingFEs comp1
    ++ noncomplyingFEs comp2
  TR _ comp1 comp2 comp3 _ ->
    noncomplyingFEs comp1
    ++ noncomplyingFEs comp2
    ++ noncomplyingFEs comp3

-- | given two preproofs, assumed to be proofs, say whether they are alpha equivalent
alphaEquivalent
  :: PreProof
  -> PreProof
  -> Bool
alphaEquivalent pp1 pp2 = alphaGo [] pp1 pp2
  where
    alphaGo :: [(Text, Text)] -> PreProof -> PreProof -> Bool
    alphaGo alph p1 p2 = case (p1, p2) of
      (Open fm1, Open fm2) -> fm1 == fm2
      (Discharged lb1 fm1, Discharged lb2 fm2) -> fm1 == fm2 && (lb1, lb2) `elem` alph
      (UR rl1 com1 fm1, UR rl2 com2 fm2)
        | fm1 /= fm2 -> False
        | otherwise -> case (rl1, rl2) of
            (II lb1, II lb2) -> alphaGo ((lb1, lb2) : alph) com1 com2
            (NI lb1, NI lb2) -> alphaGo ((lb1, lb2) : alph) com1 com2
            (CEL, CEL) -> alphaGo alph com1 com2
            (CER, CER) -> alphaGo alph com1 com2
            (DIL, DIL) -> alphaGo alph com1 com2
            (DIR, DIR) -> alphaGo alph com1 com2
            (FE, FE)   -> alphaGo alph com1 com2
            _          -> False
      (BR rl1 com11 com12 fm1, BR rl2 com21 com22 fm2)
        | fm1 /= fm2 -> False
        | rl1 /= rl2 -> False
        | otherwise  -> alphaGo alph com11 com21 && alphaGo alph com12 com22
      (TR rl1 com11 com12 com13 fm1, TR rl2 com21 com22 com23 fm2)
        | fm1 /= fm2 -> False
        | otherwise -> case (rl1, rl2) of
            (DE lb1, DE lb2) ->
              alphaGo alph com13 com23
              && alphaGo ((lb1, lb2) : alph) com12 com22
              && alphaGo ((lb1, lb2) : alph) com11 com21
      (_, _) -> False

-- | Given a preproof, returns a list of all discharge labels used by rules in the proof
labelsIn
 :: PreProof
 -> [Text]
labelsIn = \case
  Open _ -> []
  Discharged _ _ -> []
  UR ur com _ ->
    let oldLabels = labelsIn com
    in case ur of
      (II lb) -> lb : oldLabels
      (NI lb) -> lb : oldLabels
      _       -> oldLabels
  BR _ com1 com2 _ -> labelsIn com1 ++ labelsIn com2
  TR tr com1 com2 com3 _ ->
    let oldLabels = labelsIn com1 ++ labelsIn com2 ++ labelsIn com3
    in case tr of
      (DE lb) -> lb : oldLabels

-- | Given a preproof, returns a discharge label not used in that proof
freshLabel
  :: PreProof
  -> Text
freshLabel pp = pack $ show labNum
  where labNum = head $ ([1..] :: [Int]) \\ map (read . unpack) (labelsIn pp)

-- | Given a preproof, assumed to be a proof, modify it to make all applications of FE have atomic conclusions
removeCompoundFEs
  :: PreProof
  -> PreProof
removeCompoundFEs pp = case pp of
  Open _ -> pp
  Discharged _ _ -> pp
  UR FE com fm -> case fm of
    A _ -> UR FE (removeCompoundFEs com) fm
    N Falsum -> removeCompoundFEs com
    N Verum  -> error "I haven't dealt with Verum!"
    U Negation _ ->
      let newCom = removeCompoundFEs com
      in UR (NI $ freshLabel newCom) newCom fm
    B Conjunction conj1 conj2 ->
      let newCom = removeCompoundFEs com
      in BR CI (UR FE newCom conj2) (UR FE newCom conj1) fm
    -- See R11S2.1 for the following choice:
    B Disjunction disj1 _ ->
      UR DIL (UR FE (removeCompoundFEs com) disj1) fm
    B Implication _ cons ->
      let newCom = removeCompoundFEs com
      in UR (II $ freshLabel newCom) (UR FE newCom cons) fm
  UR ur com gf ->
    UR ur (removeCompoundFEs com) gf
  BR br com1 com2 gf ->
    BR br (removeCompoundFEs com1) (removeCompoundFEs com2) gf
  TR tr com1 com2 com3 gf ->
    TR tr (removeCompoundFEs com1) (removeCompoundFEs com2) (removeCompoundFEs com3) gf
