{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Logic.PreProofs where

import Import.NoFoundation ( shamlet )

import Data.Aeson (ToJSON, FromJSON)
import Data.List ( nub )
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.String (renderHtml)


import Logic.Formulas ( Connective(..)
                      , NullaryConnective(..)
                      , UnaryConnective(..)
                      , BinaryConnective(..)
                      , Formula
                      , displayFormula
                      )
import Logic.FormulaParsing.Lexer (alexScanTokens)
import Logic.FormulaParsing.Parser (happyFormula)

data RawProofTree = RawProofTree
  { label :: Text
  , rule ::  Text
  , forest :: [RawProofTree]
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Rule data types
-- Discharging rules carry Text to indicate what's discharged
data UnaryRule
  = CEL
  | CER
  | DIL
  | DIR
  | II Text
  | NI Text
  | FE
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BinaryRule
  = CI
  | IE
  | NE
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype TrinaryRule
  = DE Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Rule
  = RU UnaryRule
  | RB BinaryRule
  | RT TrinaryRule
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

governedConn :: Rule -> Connective
governedConn = \case
  (RU CEL) -> CB Conjunction
  (RU CER) -> CB Conjunction
  (RU DIL) -> CB Disjunction
  (RU DIR) -> CB Disjunction
  (RU (II _)) -> CB Implication
  (RU (NI _)) -> CU Negation
  (RU FE) -> CN Falsum

  (RB CI) -> CB Conjunction
  (RB IE) -> CB Implication
  (RB NE) -> CU Negation

  (RT (DE _)) -> CB Disjunction

-- The Formulas here are the conclusions of the rule
data PreProof
  = Open Formula
  | Discharged Text Formula
  | UR UnaryRule PreProof Formula
  | BR BinaryRule PreProof PreProof Formula
  | TR TrinaryRule PreProof PreProof PreProof Formula
  deriving (Show, Generic, ToJSON, FromJSON)

makeRaw :: PreProof -> RawProofTree
makeRaw = \case
  Open fm ->
    RawProofTree
      { label = displayFormula fm
      , rule  = ""
      , forest = []
      }
  Discharged lb fm ->
    RawProofTree
      { label = displayFormula fm
      , rule  = lb
      , forest = [ RawProofTree
                   { label = "", rule = "", forest = [] }
                 ]}
  UR ur com fm ->
    RawProofTree
      { label = displayFormula fm
      , rule  = ruleName (RU ur)
      , forest = [makeRaw com]
      }
  BR br com1 com2 fm ->
    RawProofTree
      { label = displayFormula fm
      , rule = ruleName (RB br)
      , forest = [makeRaw com1, makeRaw com2]
      }
  TR tr com1 com2 com3 fm ->
    RawProofTree
      { label = displayFormula fm
      , rule = ruleName (RT tr)
      , forest = [makeRaw com1, makeRaw com2, makeRaw com3]
      }


data RuleError
  = BadRule Text
  | MissingDischarge Rule
  | BadDischarge Rule Text
  deriving (Show, Generic, ToJSON, FromJSON)

okDischargeLabel :: Text -> Bool
okDischargeLabel tx =
  case T.find (\c -> not $ c `elem` ['0'..'9']) tx of
    Just _  -> False
    Nothing -> True

readRule :: Text -> Either RuleError Rule
readRule tx =
  case cleanText of
    "/\\I"  -> Right $ RB CI
    "/\\EL" -> Right $ RU CEL
    "/\\ER" -> Right $ RU CER
    "\\/IL" -> Right $ RU DIL
    "\\/IR" -> Right $ RU DIR
    "->E"   -> Right $ RB IE
    "_|_E"  -> Right $ RU FE
    "~E"    -> Right $ RB NE
    _       -> case T.splitAt 3 cleanText of
                  ("\\/E", dm) ->
                      if T.null dm
                        then Left $ MissingDischarge (RT $ DE "")
                      else if okDischargeLabel dm
                        then Right $ RT (DE dm)
                      else Left $ BadDischarge (RT $ DE dm) dm
                  ("->I" , dm) ->
                      if T.null dm
                        then Left $ MissingDischarge (RU $ II "")
                      else if okDischargeLabel dm
                        then Right $ RU (II dm)
                      else Left $ BadDischarge (RU $ II dm) dm
                  (pre3, rest) ->
                      let (pre2, one) = T.splitAt 2 pre3
                          dm = one <> rest
                      in if pre2 == "~I"
                          then if T.null dm
                                 then Left $ MissingDischarge (RU $ NI "")
                               else if okDischargeLabel dm
                                 then Right $ RU (NI dm)
                               else Left $ BadDischarge (RU $ NI dm) dm
                         else Left $ BadRule tx
  where
    cleanText = T.filter (/= ' ') . T.toUpper $ tx

ruleName :: Rule -> Text
ruleName = \case
  RB CI  -> "/\\I"
  RU CEL -> "/\\EL"
  RU CER -> "/\\ER"
  RU DIL -> "\\/IL"
  RU DIR -> "\\/IR"
  RT (DE tx) -> "\\/E" <> tx
  RU (II tx) -> "->I" <> tx
  RU (NI tx) -> "~I" <> tx
  RB IE  -> "->E"
  RB NE  -> "~E"
  RU FE  -> "_|_E"

ruleArity :: Rule -> Int
ruleArity = \case
  (RU _) -> 1
  (RB _) -> 2
  (RT _) -> 3

data PreProofParseError
  = CantReadRule Text
  | CantReadFormula Text
  | FormulaNoDischargeLabel Formula
  | RuleNoDischargeLabel Rule
  | BadDischargeLabel Text
  | WrongShape Rule Int
  deriving (Show, Generic, ToJSON, FromJSON)

displayPPPError :: PreProofParseError -> String
displayPPPError = \case
  CantReadRule "" -> renderHtml $ [shamlet|<p>There's a blank rule annotation!|]
  CantReadRule rl -> renderHtml $ [shamlet|<p>I can't tell what rule <code>#{rl}</code> is!|]
  CantReadFormula "" -> renderHtml $ [shamlet|<p>There's a blank where there should be a sentence!|]
  CantReadFormula fm -> renderHtml $ [shamlet|<p>I can't tell what sentence <code>#{fm}</code> is!|]
  FormulaNoDischargeLabel fm -> renderHtml $ [shamlet|<p>You can't discharge <code>#{displayFormula fm}</code> without a discharge label!|]
  RuleNoDischargeLabel rl -> renderHtml $ [shamlet|<p>You have a step of <code>#{ruleName rl}</code> that needs a discharge label, but doesn't have one!|]
  BadDischargeLabel tx -> renderHtml $ [shamlet|<p>You can't use <code>#{tx}</code> as a discharge label! Stick to digits.|]
  WrongShape rl nm -> renderHtml $ [shamlet|<p>The rule <code>#{ruleName rl}</code> needs #{ruleArity rl} subproofs; here it has #{nm}!|]

parseFmla :: Text -> Maybe Formula
parseFmla tx =
  case happyFormula . alexScanTokens . T.unpack $ tx of
    Left _   -> Nothing
    Right fm -> Just fm

readPreProof :: RawProofTree -> Either PreProofParseError PreProof
readPreProof (RawProofTree lbl "" []) =
  case parseFmla lbl of
    Nothing -> Left  $ CantReadFormula lbl
    Just fm -> Right $ Open fm
readPreProof (RawProofTree lbl rl [(RawProofTree "" "" [])]) =
  case parseFmla lbl of
    Nothing -> Left $ CantReadFormula lbl
    Just fm -> case rl of
                 "" -> Left $ FormulaNoDischargeLabel fm
                 _  -> case T.find (\c -> not $ c `elem` ['0'..'9']) rl of
                         Just _  -> Left  $ BadDischargeLabel rl
                         Nothing -> Right $ Discharged rl fm
readPreProof (RawProofTree lbl rl for) =
  case parseFmla lbl of
    Nothing -> Left $ CantReadFormula lbl
    Just fm -> case readRule rl of
      Left (BadRule _)          -> Left $ CantReadRule rl
      Left (MissingDischarge r) -> Left $ RuleNoDischargeLabel r
      Left (BadDischarge _ dm)  -> Left $ BadDischargeLabel dm
      Right r@(RU ru) ->
        case for of
          [pp1] -> do
            p1 <- readPreProof pp1
            return $ UR ru p1 fm
          subs  -> Left $ WrongShape r (length subs)
      Right r@(RB rb) ->

        case for of
          [pp1, pp2] -> do
              p1 <- readPreProof pp1
              p2 <- readPreProof pp2
              return $ BR rb p1 p2 fm
          subs       -> Left $ WrongShape r (length subs)
      Right r@(RT rt) ->
        case for of
          [pp1, pp2, pp3] -> do
              p1 <- readPreProof pp1
              p2 <- readPreProof pp2
              p3 <- readPreProof pp3
              return $ TR rt p1 p2 p3 fm
          subs            -> Left $ WrongShape r (length subs)

-- Getting information about preproofs

ppConclusion :: PreProof -> Formula
ppConclusion = \case
  Open fm -> fm
  Discharged _ fm -> fm
  UR _ _ fm -> fm
  BR _ _ _ fm -> fm
  TR _ _ _ _ fm -> fm

ppSize :: PreProof -> Int
ppSize = \case
  Open _ -> 0
  Discharged _ _ -> 0
  UR _ pp _ -> 1 + ppSize pp
  BR _ pp1 pp2 _ -> 1 + ppSize pp1 + ppSize pp2
  TR _ pp1 pp2 pp3 _ -> 1 + ppSize pp1 + ppSize pp2 + ppSize pp3

ppOpenAssumptions :: PreProof -> [Formula]
ppOpenAssumptions = nub . go
  where
    go = \case
          Open fm -> [fm]
          Discharged _ _ -> []
          UR _ pp _ -> go pp
          BR _ pp1 pp2 _ -> go pp1 <> go pp2
          TR _ pp1 pp2 pp3 _ -> go pp1 <> go pp2 <> go pp3

ppDischargedAssumptions :: PreProof -> [Formula]
ppDischargedAssumptions = nub . go
  where
    go = \case
          Open _ -> []
          Discharged _ fm -> [fm]
          UR _ pp _ -> go pp
          BR _ pp1 pp2 _ -> go pp1 <> go pp2
          TR _ pp1 pp2 pp3 _ -> go pp1 <> go pp2 <> go pp3
