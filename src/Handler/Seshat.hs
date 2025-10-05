{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Seshat where

import Foundation ( Handler
                  , Route(..)
                  )
import Import.NoFoundation ( Grouping(..)
                           , EntityField(..)
                           , Html
                           , Int64
                           , Key(..)
                           , Score(..)
                           , Text
                           , User(..)
                           , Yesod(..)
                           , (&&&)
                           , (==.)
                           , insert
                           , pack
                           , parseCheckJsonBody
                           , replace
                           , returnJson
                           , runDB
                           , selectFirst
                           , selectList
                           , setTitle
                           , unpack
                           , widgetFile
                           )
import Database.Esqueleto.Legacy
    ( Entity(Entity), PersistEntity(Key), BackendKey(unSqlBackendKey, SqlBackendKey) )

import Text.Julius (RawJS (..))
import Data.Aeson (Result(..), Value(..), (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Text (stripPrefix)
import Text.Read (readMaybe)

import Control.Monad (join)
import qualified Data.List as L
import qualified Data.Map as M

import Settings.Binchicken (activeExerciseTypes)
import Scoring
  ( calculateSummary
  , displayPoints
  , exScore
  , tally
  , totalPoints
  , Summary(..)
  )

updateTag :: Text
updateTag = "update-"

displayUserId :: Key User -> Text
displayUserId = pack . show . unSqlBackendKey . unUserKey

parseUserUpdate :: Text -> Maybe (Key User)
parseUserUpdate s = do
  dropped <- stripPrefix updateTag s
  i <- (readMaybe (unpack dropped) :: Maybe Int64)
  return $ UserKey (SqlBackendKey i)

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

seshatFormIds :: (Text, Text, Text, Text, Text)
seshatFormIds = ("yearInput", "sectionInput", "submitButton", "updateChecked", "updateMsg")

getSeshatR :: Handler Html
getSeshatR = do
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (scs :: [Entity Score]) <- runDB $ selectList [] []
  (grs :: [Entity Grouping]) <- runDB $ selectList [] []
  let summ = calculateSummary $ tally usrs scs
      exts = activeExerciseTypes
      groups = alignGroupings usrs grs
      yearList :: [(Text, Int)]
      yearList = map (pack . show &&& id) [2025, 2026]
      (yearInputId, sectionInputId, submitButtonId, updateCheck, updateMsgId) = seshatFormIds
  defaultLayout $ do
    setTitle "Seshat"
    $(widgetFile "seshat")

-- | updates a grouping with new year and section
-- | Nothing values passed in result in status quo, not updates to clear
updateGroup
  :: Grouping
  -> Maybe Int
  -> Maybe Int
  -> Grouping
updateGroup g my ms =
  let g1 = case my of
             Nothing -> g
             jy@(Just _) -> g { groupingYear = jy }
      g2 = case ms of
             Nothing -> g1
             js@(Just _) -> g1 { groupingSection = js }
  in g2

updateGrouping
  :: Text
  -> Maybe Int
  -> Maybe Int
  -> Handler ()
updateGrouping tx my ms =
    let mk = parseUserUpdate tx
    in case mk of
         Nothing -> return ()
         Just k -> do
           currGrp <- runDB $ selectFirst [GroupingUserId ==. k] []
           case currGrp of
             Nothing -> do
               let newGrp = Grouping { groupingUserId = k
                                     , groupingYear = my
                                     , groupingSection = ms
                                     , groupingEscapeGroup = Nothing
                                     , groupingMurderGroup = Nothing
                                     }
               _ <- runDB $ insert newGrp
               return ()
             Just (Entity gid g) ->
               do
                 let newGrp = updateGroup g my ms
                 _ <- runDB $ replace gid newGrp
                 return ()

postSeshatR :: Handler Value
postSeshatR = do
    rRequestJSON <- (parseCheckJsonBody :: Handler (Result Value))
    case rRequestJSON of
      Error s -> returnJson s -- Did we get a parseable response?
      Success requestJson -> case requestJson of
        Object hm -> -- is the response an Object?
          let (myear :: Maybe Int) = parseMaybe id (hm .: "updateYear")
              (msec :: Maybe Int) = parseMaybe id (hm .: "updateSection")
              (musers :: Maybe [Text]) = parseMaybe id (hm .: "updateUsers")
              usrs = case musers of
                       Nothing -> []
                       Just us -> us
          in do
            mapM_ (\t -> updateGrouping t myear msec) usrs
            returnJson ("Year: " <> (show myear) <> " Section: " <> (show msec) <> " Users: " <> (show musers))
        _ -> returnJson ("Something went wrong!" :: Text) -- the response was JSON but not an Object
