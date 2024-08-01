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
      Exercise(..),
      Key,
      Maybe(..),
      Value,
      Generic,
      IO,
      Int,
      SentExercise(..),
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
      getCurrentTime,
      hamlet,
      insert,
      julius,
      lucius,
      maybeAuthId,
      whamlet,
      toWidget,
      newIdent,
      not,
      map,
      runDB,
      toHtml,
      toStrict,
      return,
      error,
      length,
      (-))
import Text.Julius (RawJS (..))
import Data.Aeson (Object, Result(..), (.=), (.:), decodeStrict, encode, object, toJSON, ToJSON, FromJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (parse)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.Random as SR

import Handler.LoginCheck (loginNotifyW)
import Logic.Formulas (Atomic(..), displayAtomic, displayFormula)
import Logic.Random (randomArgumentIO)
import Settings.Binchicken (RandomArgumentSettings(..))
import Logic.Valuations (ValDisplay(..), displayVal, objToValuation)
import Logic.Arguments (Argument(..), atomsInArg)
import Logic.Matrices
  ( IsCounterexample(..),
    displayMMVHtml,
    displayUPsHTML,
    displayDCHTML,
    MatrixInfo(..),
    MysteryMatrixValuation(..),
    MatrixTag(..),
    isValidMV,
    IsValidMV(..),
    isCexMV,
    displayNameMatrix )


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

data ICEXAttempt =
  ICEXAttempt { icexExerciseId :: Key Exercise
              , icexResponse :: MysteryMatrixValuation
              } deriving (Generic)

instance ToJSON ICEXAttempt
instance FromJSON ICEXAttempt

decodeCex :: Text -> Maybe (Argument, MatrixTag)
decodeCex tx = decodeStrict $ encodeUtf8 tx

encodeCex
  :: Argument
  -> MatrixTag
  -> Text
encodeCex arg mt = decodeUtf8 . toStrict $ encode (arg, mt)

getCounterexample
  :: RandomArgumentSettings     -- ^ settings for the random argument
  -> [MatrixInfo]               -- ^ a list of matrices; one will be chosen at random
  -> ExerciseType               -- ^ the type of the generated exercise
  -> Text                       -- ^ title for the generated page
  -> Text                       -- ^ header text
  -> Text                       -- ^ any extra instructional text
  -> Route BinChicken           -- ^ Route to send AJAX to
  -> Handler Html
getCounterexample ras mats etype title headr extraText ajaxRoute = do
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
        ex = Exercise { exerciseExerciseType = etype
                      , exerciseExerciseContent = encodeCex arg mtag
                      }
    exid <- runDB $ insert ex
    maybeCurrentUserId <- maybeAuthId
    case maybeCurrentUserId of
      Just uid -> do
        now <- liftIO getCurrentTime
        let sent = SentExercise { sentExerciseUserId = Just uid
                                , sentExerciseExerciseId = exid
                                , sentExerciseSentAt = Just now
                                }
        _ <- runDB $ insert sent
        defaultLayout $ do
          setTitle (toHtml title)
          $(widgetFile "counterexample")
      Nothing -> defaultLayout $ do
        setTitle (toHtml title)
        $(widgetFile "counterexample")


data ParsedObject =
  ParsedObject
    { poArgument :: Argument
    , poTag :: MatrixTag
    , poMMValuation :: Maybe MysteryMatrixValuation
    }

maybeResult :: Result a -> Maybe a
maybeResult (Error _)   = Nothing
maybeResult (Success s) = Just s

processPost' :: Object -> Maybe ParsedObject
processPost' obj = do
  arg <- maybeResult $ parse (.: "incArgument") obj
  matTag <- maybeResult $ parse (.: "incMatrix") obj
  hasVal <- maybeResult $ parse (.: "icexHasValuation") obj
  if not hasVal
    then return $ ParsedObject arg matTag Nothing
    else do
      valObj <- maybeResult $ (parse (.: "incValuation") obj :: Result Object)
      let mystval = case matTag of
                      ClassicalTag  -> MClassical (objToValuation valObj)
                      K3Tag -> MK3 (objToValuation valObj)
                      LPTag -> MLP (objToValuation valObj)
                      FDETag -> MFDE (objToValuation valObj)
      return $ ParsedObject arg matTag (Just mystval)

-- | Given a response Object and an Exercise, attempt to digest them into a ParsedObject
processExercise
  :: Object
  -> Exercise
  -> Result ParsedObject
processExercise obj ex = do
  hasVal <- parse (.: "icexHasValuation") obj
  case decodeCex $ exerciseExerciseContent ex of
    Nothing -> Error "Couldn't read exercise content!"
    Just (arg, mtag) ->
      if not hasVal
      then Success $ ParsedObject arg mtag Nothing
      else do
        valObj <- (parse (.: "icexValuation") obj :: Result Object)
        let mystval = case mtag of
              ClassicalTag -> MClassical (objToValuation valObj)
              K3Tag        -> MK3 (objToValuation valObj)
              LPTag        -> MLP (objToValuation valObj)
              FDETag       -> MFDE (objToValuation valObj)
        Success $ ParsedObject arg mtag (Just mystval)



prepareResponse :: Key Exercise -> ParsedObject -> (Value, Attempt)
prepareResponse exid (ParsedObject arg matTag Nothing) = (responseObj, att)
  where
    (corr, responseObj) =
      case isValidMV matTag arg of
             ValidMV ->
               ( True
               , object [ "rval" .= toJSON (Nothing :: Maybe Void)
                        , "report" .= toJSON ValidCorrect
                        ]
               )
             HasCounterexampleMV mcex ->
               ( False
               , object [ "rval" .= toJSON (Nothing :: Maybe Void)
                        , "report" .= toJSON ValidIncorrect
                        , "cex" .= toJSON (displayMMVHtml mcex)
                        ]
               )
    justTextify = Just . toStrict . encodeToLazyText
    att = Attempt { attemptUserId = Nothing
                  , attemptExerciseId = exid
                  , attemptIsCorrect = corr
                  , attemptSubmittedResponse =
                      justTextify $ object [ "response" .= toJSON ("Valid" :: Text)]
                  , attemptSubmittedAt = Nothing
                  }
prepareResponse exid (ParsedObject arg _ (Just mystVal)) = (responseObj, att)
  where
    (corr, responseObj) =
      case isCexMV mystVal arg of
            Nothing ->
              ( False
              , object [ "rval" .=  toJSON mystVal
                       , "report" .= toJSON CounterexampleIncomplete
                       ]
              )
            Just Counterexamples ->
              ( True
              , object [ "rval" .= toJSON mystVal
                       , "report" .= toJSON CounterexampleCorrect
                       ]
              )
            Just (UPsAndDCs ups mdc) ->
              ( False
              , object [ "rval" .= toJSON mystVal
                       , "report" .= toJSON CounterexampleIncorrect
                       , "ups" .= toJSON (displayUPsHTML ups)
                       , "dc"  .= toJSON (displayDCHTML mdc)
                       ]
              )
    justTextify = Just . toStrict . encodeToLazyText
    att = Attempt { attemptUserId = Nothing
                  , attemptExerciseId = exid
                  , attemptIsCorrect = corr
                  , attemptSubmittedResponse =
                      justTextify $ object [ "response" .= toJSON (HasCounterexampleMV mystVal) ]
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
