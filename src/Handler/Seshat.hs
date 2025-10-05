{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Seshat where

import Foundation ( Handler
                  , Route(..)
                  , Widget
                  )
import Import.NoFoundation ( AForm
                           , FormResult(..)
                           , Grouping(..)
                           , Html
                           , Key(..)
                           , MForm
                           , Score(..)
                           , Text
                           , User(..)
                           , Value
                           , Yesod(..)
                           , (&&&)
                           , aopt
                           , check
                           , generateFormPost
                           , intField
                           , pack
                           , renderDivs
                           , runDB
                           , selectFieldList
                           , selectList
                           , setTitle
                           , widgetFile
                           )
import Database.Esqueleto.Legacy
    ( Entity(Entity), PersistEntity(Key), BackendKey(unSqlBackendKey) )

import Control.Monad (join)
import qualified Data.List as L
import qualified Data.Map as M

import Settings.Binchicken (activeExerciseTypes, Year(..))
import Scoring
  ( calculateSummary
  , displayPoints
  , exScore
  , tally
  , totalPoints
  , Summary(..)
  )

data GroupingInput = GI (Maybe Year) (Maybe Int)

groupingAForm :: AForm Handler GroupingInput
groupingAForm = GI
  <$> aopt (selectFieldList years) "Year" Nothing
  <*> aopt sectionField "Section" Nothing
  where
    years :: [(Text, Year)]
    years = map (pack . show &&& id) [minBound..maxBound]
    sectionField = check validateSection intField
    validateSection s
      | s < 0 = Left ("No negative sections!" :: Text)
      | s > 99 = Left "Are there really 100 sections?"
      | otherwise = Right s

groupingForm :: Html -> MForm Handler (FormResult GroupingInput, Widget)
groupingForm = renderDivs groupingAForm

updateTag :: String
updateTag = "update"

displayUserId :: Key User -> String
displayUserId = show . unSqlBackendKey . unUserKey

displayYear :: Maybe Int -> String
displayYear Nothing = "*"
displayYear (Just y) = show y

displaySection :: Maybe Int -> String
displaySection = displayYear

alignGroupings :: [Entity User]
               -> [Entity Grouping]
               -> [(Key User, Maybe Grouping)]
alignGroupings [] _ = []
alignGroupings ((Entity i u) : eus) egs = (go i egs) : alignGroupings eus egs
  where
    go :: Key User -> [Entity Grouping] -> (Key User, Maybe Grouping)
    go i [] = (i, Nothing)
    go i ((Entity _ gg) : egs)
      | i == groupingUserId gg = (i, Just gg)
      | otherwise = go i egs

grYearSection:: [(Key User, Maybe Grouping)] -> Key User -> (Maybe Int, Maybe Int)
grYearSection grs i =
  let mmg = join $ L.lookup i grs
  in case mmg of
        Nothing -> (Nothing, Nothing)
        Just g -> (groupingYear g, groupingSection g)

displayScore :: Maybe Int -> String
displayScore =
  \case Nothing -> "Missing"
        Just i  -> show i

getSeshatR :: Handler Html
getSeshatR = do
  (gFormWidget, enctype) <- generateFormPost groupingForm
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (scs :: [Entity Score]) <- runDB $ selectList [] []
  (grs :: [Entity Grouping]) <- runDB $ selectList [] []
  let summ = calculateSummary $ tally usrs scs
      exts = activeExerciseTypes
      groups = alignGroupings usrs grs
  defaultLayout $ do
    setTitle "Seshat"
    $(widgetFile "seshat")

postSeshatR :: Handler Value
postSeshatR = undefined
