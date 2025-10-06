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
                           , liftIO
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
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseMaybe)
import Data.Text (stripPrefix)
import Text.Read (readMaybe, Lexeme (String))

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

displayEscape :: Maybe Int -> String
displayEscape = displayYear

displayMurder :: Maybe Int -> String
displayMurder = displayYear

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

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

thd4 :: (a, b, c, d) -> c
thd4 (_, _, x, _) = x

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, x) = x

grYSEM:: [(Key User, Maybe Grouping)] -> Key User -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
grYSEM grs i =
  let mmg = join $ L.lookup i grs
  in case mmg of
        Nothing -> (Nothing, Nothing, Nothing, Nothing)
        Just g -> (groupingYear g, groupingSection g, groupingEscapeGroup g, groupingMurderGroup g)

displayScore :: Maybe Int -> String
displayScore =
  \case Nothing -> "Missing"
        Just i  -> show i

seshatFormIds :: (Text, Text, Text, Text, Text, Text, Text)
seshatFormIds = ("yearInput", "sectionInput", "submitButton", "updateChecked", "updateMsg", "escapeGroup", "murderGroup")

getSeshatR :: Handler Html
getSeshatR = do
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (scs :: [Entity Score]) <- runDB $ selectList [] []
  (grs :: [Entity Grouping]) <- runDB $ selectList [] []
  let (uy, us, ue, um, uu) = updateTags
      summ = calculateSummary $ tally usrs scs
      exts = activeExerciseTypes
      groups = alignGroupings usrs grs
      yearList :: [(Text, Int)]
      yearList = map (pack . show &&& id) [2025, 2026]
      (yearInputId, sectionInputId, submitButtonId, updateCheck, updateMsgId, escapeGrpId, murderGrpId) = seshatFormIds
  defaultLayout $ do
    setTitle "Seshat"
    $(widgetFile "seshat")

-- | updates a grouping with new year, section, escape, murder
-- | Nothing values passed in result in status quo, not updates to clear
updateGroup
  :: Grouping
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Grouping
updateGroup g my ms me mm =
  let g1 = case my of
             Nothing -> g
             jy@(Just _) -> g { groupingYear = jy }
      g2 = case ms of
             Nothing -> g1
             js@(Just _) -> g1 { groupingSection = js }
      g3 = case me of
             Nothing -> g2
             je@(Just _) -> g2 { groupingEscapeGroup = je }
      g4 = case mm of
             Nothing -> g3
             jm@(Just _) -> g3 { groupingMurderGroup = jm }
  in g4

updateGrouping
  :: Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Handler ()
updateGrouping tx my ms me mm =
    let mk = parseUserUpdate tx
    in case mk of
         Nothing -> return ()
         Just k -> do
           currGrp <- runDB $ selectFirst [GroupingUserId ==. k] []
           case currGrp of
             Nothing -> do
               liftIO $ putStrLn "No existing grouping found"
               let newGrp = Grouping { groupingUserId = k
                                     , groupingYear = my
                                     , groupingSection = ms
                                     , groupingEscapeGroup = me
                                     , groupingMurderGroup = mm
                                     }
               _ <- runDB $ insert newGrp
               return ()
             Just (Entity gid g) ->
               do
                 liftIO $ putStrLn "Grouping found"
                 let newGrp = updateGroup g my ms me mm
                 _ <- runDB $ replace gid newGrp
                 return ()

updateTags :: (Text, Text, Text, Text, Text)
updateTags = ("updateYear", "updateSection", "updateEscape", "updateMurder", "updateUsers")

postSeshatR :: Handler Value
postSeshatR = do
    rRequestJSON <- (parseCheckJsonBody :: Handler (Result Value))
    case rRequestJSON of
      Error s -> returnJson s -- Did we get a parseable response?
      Success requestJson -> case requestJson of
        Object hm -> -- is the response an Object?
          let (uy, us, ue, um, uu) = updateTags
              (myear :: Maybe String) = parseMaybe id (hm .: fromText uy)
              (msec :: Maybe String) = parseMaybe id (hm .: fromText us)
              (mesc :: Maybe String) = parseMaybe id (hm .: fromText ue)
              (mmur :: Maybe String) = parseMaybe id (hm .: fromText um)
              (musers :: Maybe [Text]) = parseMaybe id (hm .: fromText uu)
              usrs = case musers of
                       Nothing -> []
                       Just uss -> uss
          in do
            mapM_ (\t -> do
                           updateGrouping t (myear >>= readMaybe) (msec >>= readMaybe) (mesc >>= readMaybe) (mmur >>= readMaybe)
                           case (parseUserUpdate t) of
                             Nothing -> liftIO . putStrLn . unpack $ ("Couldn't parse " <> t)
                             Just k -> liftIO . putStrLn . unpack $ ("Parsed " <> (displayUserId k))
                  ) usrs
            returnJson ("Year: " <> (show myear)
                        <> " Section: " <> (show msec)
                        <> " Escape: " <> (show mesc)
                        <> " Murder: " <> (show mmur)
                        <> " Users: " <> (show musers))
        _NotAnObject -> returnJson ("Something went wrong!" :: Text) -- the response was JSON but not an Object
