{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Counterexamples where

import ExerciseType (ExerciseType(..))
import Foundation (Route(..), Handler, Widget, BinChicken)
import Import.NoFoundation
    ( ($),
      (<>),
      Maybe(..),
      Value,
      Generic,
      IO,
      Int,
      Text,
      Bool(..),
      MonadIO(liftIO),
      setTitle,
      (.),
      Html,
      Void,
      Yesod(defaultLayout),
      widgetFile,
      Attempt(..),
      LazySequence (toStrict),
      hamlet,
      julius,
      lucius,
      whamlet,
      toWidget,
      newIdent,
      not,
      map,
      toHtml,
      return,
      error,
      length,
      (-))
import Text.Julius (RawJS (..))
import Data.Aeson (Object, Result(..), (.=), (.:), object, toJSON, ToJSON, FromJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (parse)
import qualified System.Random as SR

import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas (Atomic(..), displayAtomic, displayFormula)
import Logic.Random (randomArgumentIO)
import Settings.Binchicken (RandomArgumentSettings(..))
import Logic.Valuations (Valuation, ValDisplay(..), displayVal, displayValuationHtml, objToValuation)
import Logic.Arguments (Argument(..), atomsInArg)
import Logic.Matrices (IsCounterexample(..), IsValid(..), displayUPsHTML, displayDCHTML, MatrixInfo(..), ClassicalMatrix, K3Matrix, LPMatrix, FDEMatrix, MysteryValuation(..), MatrixTag(..), isValidMV, IsValidMV(..), isCexMV, displayNameMatrix)


data CexOutcome =
    ValidCorrect
  | ValidIncorrect
  | CounterexampleIncomplete
  | CounterexampleCorrect
  | CounterexampleIncorrect
  deriving (Generic, ToJSON, FromJSON)

(!!) :: [a] -> Int -> a
[] !! _ = error "Empty list!"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

randomElement :: [a] -> IO a
randomElement xs = do
  ix <- SR.getStdRandom (SR.randomR (0, length xs - 1))
  return (xs !! ix)

getCounterexample
  :: RandomArgumentSettings     -- ^ settings for the random argument
  -> [MatrixInfo]               -- ^ a list of matrices; one will be chosen at random
  -> Text                       -- ^ title for the generated page
  -> Text                       -- ^ header text
  -> Text                       -- ^ any extra instructional text
  -> Route BinChicken           -- ^ Route to send AJAX to
  -> Handler Html
getCounterexample ras mats title headr extraText ajaxRoute = do
    let displayArgumentId = "js-display-argument" :: Text
        inputValuationId = "js-input-valuation" :: Text
        displayResultId = "js-display-result" :: Text
        validButtonId = "js-valid-button" :: Text
        submitValButtonId = "js-submit-button" :: Text
    arg@(Argument prems conc) <- liftIO $ randomArgumentIO ras
    (MatrixInfo vls mtag) <- liftIO $ randomElement mats
    let ats = atomsInArg arg
        atsJs = toJSON . map displayAtomic $ ats
        valsToChooseFrom = vls
        matrxTag = displayNameMatrix mtag
    defaultLayout $ do
        setTitle (toHtml title)
        $(widgetFile "counterexample")


data ParsedObject =
  ParsedObject
    { poArgument :: Argument
    , poTag :: MatrixTag
    , poMMValuation :: Maybe MysteryValuation
    }

maybeResult :: Result a -> Maybe a
maybeResult (Error _)   = Nothing
maybeResult (Success s) = Just s

processPost :: Object -> Maybe ParsedObject
processPost obj = do
  arg <- maybeResult $ parse (.: "incArgument") obj
  hasVal <- maybeResult $ parse (.: "incHasValuation") obj
  matTag <- maybeResult $ parse (.: "incMatrix") obj
  if not hasVal
    then return $ ParsedObject arg matTag Nothing
    else do
      valObj <- maybeResult $ (parse (.: "incValuation") obj :: Result Object)
      let mystval = case matTag of
                      ClassicalTag  -> MysteryValuation (objToValuation valObj :: Valuation ClassicalMatrix)
                      K3Tag -> MysteryValuation (objToValuation valObj :: Valuation K3Matrix)
                      LPTag -> MysteryValuation (objToValuation valObj :: Valuation LPMatrix)
                      FDETag -> MysteryValuation (objToValuation valObj :: Valuation FDEMatrix)
      return $ ParsedObject arg matTag (Just mystval)

prepareResponse :: ExerciseType -> ParsedObject -> (Value, Attempt)
prepareResponse exType (ParsedObject arg matTag Nothing) = (responseObj, att)
  where
    (corr, responseObj) =
      case isValidMV matTag arg of
             ValidMV ->
               ( True
               , object [ "rval" .= toJSON (Nothing :: Maybe Void)
                        , "report" .= toJSON ValidCorrect
                        ]
               )
             HasCounterexampleMV (MysteryValuation val) ->
               ( False
               , object [ "rval" .= toJSON (Nothing :: Maybe Void)
                        , "report" .= toJSON ValidIncorrect
                        , "cex" .= toJSON (displayValuationHtml val)
                        ]
               )
    justTextify = Just . toStrict . encodeToLazyText
    att = Attempt { attemptUserId = Nothing
                  , attemptExerciseType = exType
                  , attemptIsCorrect = corr
                  , attemptExerciseContent =
                      justTextify $ object [ "argument" .= toJSON arg
                                           , "matrix" .= toJSON matTag
                                           ]
                  , attemptSubmittedResponse =
                      justTextify $ object [ "response" .= toJSON ("Valid" :: Text)]
                  , attemptSubmittedAt = Nothing
                  }
prepareResponse exType (ParsedObject arg matTag (Just mystVal@(MysteryValuation actualVal))) = (responseObj, att)
  where
    (corr, responseObj) =
      case isCexMV mystVal arg of
            Nothing ->
              ( False
              , object [ "rval" .=  toJSON actualVal
                       , "report" .= toJSON CounterexampleIncomplete
                       ]
              )
            Just Counterexamples ->
              ( True
              , object [ "rval" .= toJSON actualVal
                       , "report" .= toJSON CounterexampleCorrect
                       ]
              )
            Just (UPsAndDCs ups mdc) ->
              ( False
              , object [ "rval" .= toJSON actualVal
                       , "report" .= toJSON CounterexampleIncorrect
                       , "ups" .= toJSON (displayUPsHTML ups)
                       , "dc"  .= toJSON (displayDCHTML mdc)
                       ]
              )
    justTextify = Just . toStrict . encodeToLazyText
    att = Attempt { attemptUserId = Nothing
                  , attemptExerciseType = exType
                  , attemptIsCorrect = corr
                  , attemptExerciseContent =
                      justTextify $ object [ "argument" .= toJSON arg
                                           , "matrix" .= toJSON matTag
                                           ]
                  , attemptSubmittedResponse =
                      justTextify $ object [ "response" .= toJSON (HasCounterexample actualVal) ]
                  , attemptSubmittedAt = Nothing
                  }


-- | An id for an atom, used to display current value
-- | Prefix split out for use in Julius
atomDisplayValIdPrefix :: Text
atomDisplayValIdPrefix = "js-value-for-"

atomDisplayValId :: Atomic -> Text
atomDisplayValId (At t) = atomDisplayValIdPrefix <> t


-- | A single button for setting the value of an atomic sentence
atValWidget
  :: ValDisplay v
  => v      -- ^ The value this button should set
  -> Atomic -- ^ The atomic the value is set for
  -> Widget
atValWidget vlu at = do
  let vdid = atomDisplayValId at
  valButtonId <- newIdent
  toWidget [hamlet|
              <td .oblang>
                <button .btn.btn-primary ##{valButtonId}>#{displayVal vlu}
           |]
  toWidget [julius|
                $("#{rawJS $ "#" <> valButtonId}").click(function() {
                    document.getElementById("#{rawJS vdid}").innerHTML = #{displayVal vlu};
                });
           |]
    
valWidget :: ValDisplay v => [v] -> Atomic -> Widget
valWidget vlus at = do
  let vdid = atomDisplayValId at
  toWidget [whamlet|
            <tr>
              <td .oblang.atomic>#{displayAtomic at}
              $forall vlu <- vlus
                ^{atValWidget vlu at}
              <td>
                <p .atomic-value ##{vdid}>
           |]
  toWidget [lucius|
              .atom { padding: 0px 20px; }
           |]
