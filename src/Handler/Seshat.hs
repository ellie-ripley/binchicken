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
                           , Grouping(..)
                           , EntityField(..)
                           , FormResult(..)
                           , Html
                           , Int64
                           , Key(..)
                           , MForm
                           , Score(..)
                           , Text
                           , User(..)
                           , Yesod(..)
                           , (&&&)
                           , (==.)
                           , areq
                           , checkBoxField
                           , generateFormPost
                           , insert
                           , liftIO
                           , pack
                           , parseCheckJsonBody
                           , redirect
                           , renderTable
                           , replace
                           , returnJson
                           , runDB
                           , runFormPost
                           , selectFirst
                           , selectList
                           , setTitle
                           , unpack
                           , widgetFile
                           )
import Database.Esqueleto.Legacy
    ( Entity(Entity), BackendKey(unSqlBackendKey, SqlBackendKey) )

import Text.Julius (RawJS (..))
import Data.Aeson (Result(..), Value(..), (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Csv as C
import Data.Text (stripPrefix)
import Data.Text.Encoding (decodeUtf8)
import Text.Read (readMaybe)

import Control.Monad (join)
import qualified Data.List as L
import qualified Data.Map as M

import ExerciseType (ExerciseType(..))
import Settings.Binchicken
  ( ActiveET(..)
  , activeExerciseTypes
  )
import Scoring
  ( calculateSummary
  , exScore
  , renderPoints
  , renderSummary
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
alignGroupings ((Entity i _) : eus) egs = (go i egs) : alignGroupings eus egs
  where
    go :: Key User -> [Entity Grouping] -> (Key User, Maybe Grouping)
    go ku [] = (ku, Nothing)
    go ku ((Entity _ gg) : eggs)
      | ku == groupingUserId gg = (ku, Just gg)
      | otherwise = go ku eggs

grYearSection:: [(Key User, Maybe Grouping)] -> Key User -> (Maybe Int, Maybe Int)
grYearSection grs i =
  let mmg = join $ L.lookup i grs
  in case mmg of
        Nothing -> (Nothing, Nothing)
        Just g -> (groupingYear g, groupingSection g)

displayScore :: Maybe Rational -> String
displayScore =
  \case Nothing -> "Missing"
        Just i  -> renderPoints i

seshatFormIds :: (Text, Text, Text, Text, Text, Text)
seshatFormIds = ("yearInput", "sectionInput", "submitButton", "updateChecked", "updateMsg", "csv")

processAETs :: [ActiveET] -> [(Int, Maybe ExerciseType)]
processAETs aets = map go (zip [(1::Int)..] aets)
  where
    go (i, aet) = case aet of
                    Placeholder -> (i, Nothing)
                    Active et   -> (i, Just et)
    

getSeshatR :: Handler Html
getSeshatR = do
  (usrs :: [Entity User]) <- runDB $ selectList [] []
  (scs :: [Entity Score]) <- runDB $ selectList [] []
  (grs :: [Entity Grouping]) <- runDB $ selectList [] []
  let summ = calculateSummary $ tally usrs scs
      exNums = processAETs activeExerciseTypes
      groups = alignGroupings usrs grs
      yearList :: [(Text, Int)]
      yearList = map (pack . show &&& id) [2025, 2026]
      (yearInputId, sectionInputId, submitButtonId, updateCheck, updateMsgId, csv) = seshatFormIds
  (csvWidget, enctype) <- generateFormPost csvForm
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
               liftIO $ putStrLn "No existing grouping found"
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
                 liftIO $ putStrLn "Grouping found"
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
          let (myear :: Maybe String) = parseMaybe id (hm .: "updateYear")
              (msec :: Maybe String) = parseMaybe id (hm .: "updateSection")
              (musers :: Maybe [Text]) = parseMaybe id (hm .: "updateUsers")
              usrs = case musers of
                       Nothing -> []
                       Just us -> us
          in do
            mapM_ (\t -> do
                           updateGrouping t (myear >>= readMaybe) (msec >>= readMaybe)
                           case (parseUserUpdate t) of
                             Nothing -> liftIO . putStrLn . unpack $ ("Couldn't parse " <> t)
                             Just k -> liftIO . putStrLn . unpack $ ("Parsed " <> (displayUserId k))
                  ) usrs
            returnJson ("Year: " <> (show myear) <> " Section: " <> (show msec) <> " Users: " <> (show musers))
        _notAnObject -> returnJson ("Something went wrong!" :: Text) -- the response was JSON but not an Object


data CsvOpts = CsvOpts
  { includeEmpty :: !Bool
  } deriving (Eq, Show)


csvAForm :: AForm Handler CsvOpts
csvAForm = CsvOpts
  <$> areq checkBoxField "Include users with no attempts" (Just False)

csvForm :: Html -> MForm Handler (FormResult CsvOpts, Widget)
csvForm = renderTable csvAForm

postSeshatCSVR :: Handler Text
postSeshatCSVR = do
  ((result, _widget), _enctype) <- runFormPost csvForm
  case result of
    FormSuccess _csvOpts -> do
        (usrs :: [Entity User]) <- runDB $ selectList [] []
        (scs :: [Entity Score]) <- runDB $ selectList [] []
        let summList = map snd (M.toList . unSummary . renderSummary . calculateSummary $ tally usrs scs)
        return (decodeUtf8 . LB.toStrict . C.encodeDefaultOrderedByName $ summList)
    _notSuccess -> redirect SeshatR
