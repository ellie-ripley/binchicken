{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Murder where

import Foundation (Route(..), Handler, Widget)
import Import.NoFoundation ( AForm
                           , Bool(..)
                           , Bounded(..)
                           , Entity(..)
                           , Enum
                           , Eq(..)
                           , FormResult(..)
                           , Html
                           , Maybe(..)
                           , MForm
                           , MurderMystery(..)
                           , IO
                           , Ord
                           , Read
                           , Show(..)
                           , Text
                           , ($)
                           , (.)
                           , (<>)
                           , (>>=)
                           , (<$>)
                           , (<*>)
                           , (&&&)
                           , (==.)
                           , areq
                           , defaultLayout
                           , error
                           , generateFormPost
                           , getCurrentTime
                           , getEntity
                           , id
                           , insertEntity
                           , liftIO
                           , lucius
                           , map
                           , maybeAuthId
                           , pack
                           , redirect
                           , renderTable
                           , return
                           , runDB
                           , runFormPost
                           , selectFirst
                           , selectFieldList
                           , setTitle
                           , toStrict
                           , toWidget
                           , undefined
                           , whamlet
                           , widgetFile
                           )
import Model (EntityField(..))

import Handler.LoginCheck (loginNotifyW)

import Data.Aeson (FromJSON, ToJSON, Result(..), Value(..), (.:), decodeStrict, encode)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (catMaybes)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)

data Suspect =
    Butler
  | Cook
  | Driver
  | Gardiner
  | Killin
  deriving (Eq, Ord, Bounded, Enum, Read, Show, Generic)
instance ToJSON Suspect
instance FromJSON Suspect

suspectName :: Suspect -> Text
suspectName = \case
  Butler -> "Judith Butler"
  Cook -> "Roy Cook"
  Driver -> "Julia Driver"
  Gardiner -> "Georgi Gardiner"
  Killin -> "Anton Killin"

data Weapon =
    Argument
  | Article
  | Book
  | Demon
  | Idea
  deriving (Eq, Ord, Bounded, Enum, Read, Show, Generic)
instance ToJSON Weapon
instance FromJSON Weapon

weaponVerb :: Weapon -> Text
weaponVerb = \case
  Argument -> "argued Ellie to death"
  Article -> "gave Ellie paper cuts with an article"
  Book -> "bludgeoned Ellie with a book"
  Demon -> "snuck a demonic symbol onto Ellie's whiteboard"
  Idea -> "blew Ellie's mind with a new idea"

data Location =
    BilliardRoom
  | Conservatory
  | Kitchen
  | Library
  | Study
  deriving (Eq, Ord, Bounded, Enum, Read, Show, Generic)
instance ToJSON Location
instance FromJSON Location

locationName :: Location -> Text
locationName = \case
  BilliardRoom -> "billiard room"
  Conservatory -> "conservatory"
  Kitchen -> "kitchen"
  Library -> "library"
  Study -> "study"

data MurderGuess = MurderGuess
  { mgSuspect :: !Suspect
  , mgWeapon :: !Weapon
  , mgLocation :: !Location
  }
  deriving (Eq, Show, Generic)
instance ToJSON MurderGuess
instance FromJSON MurderGuess

-- | want (and rely on): decodeMG (encodeMG mg) == Just mg, for all guesses mg
decodeMG :: Text -> Maybe MurderGuess
decodeMG tx = decodeStrict $ encodeUtf8 tx

encodeMG :: MurderGuess -> Text
encodeMG mg = decodeUtf8 . toStrict $ encode mg

murderAForm :: AForm Handler MurderGuess
murderAForm = MurderGuess
  <$> areq (selectFieldList suspects) "The murderer is " Nothing
  <*> areq (selectFieldList weapons) " who " Nothing
  <*> areq (selectFieldList locations) " in the " Nothing
  where
    suspects :: [(Text, Suspect)]
    suspects = map (suspectName &&& id) [minBound..maxBound]
    weapons :: [(Text, Weapon)]
    weapons = map (weaponVerb &&& id) [minBound..maxBound]
    locations :: [(Text, Location)]
    locations = map (locationName &&& id) [minBound..maxBound]

murderForm :: Html -> MForm Handler (FormResult MurderGuess, Widget)
murderForm = renderTable murderAForm

correctClass :: Text
correctClass = "correct"

incorrectClass :: Text
incorrectClass = "incorrect"

attemptCSSClasses :: MurderGuess -> (Bool, Bool, Bool) ->  (Text, Text, Text)
attemptCSSClasses (MurderGuess sus wea loc) (cs, cw, cl) = (csus, cwea, cloc)
  where
    tshow :: Show a => a -> Text
    tshow = pack . show
    go t b = if b then t <> " " <> correctClass
                  else t <> " " <> incorrectClass
    csus = go ("suspect-name " <> tshow sus) cs
    cwea = go ("weapon-verb " <> tshow wea) cw
    cloc = go ("location-name " <> tshow loc) cl


attemptRow :: MurderGuess -> MurderGuess -> Widget
attemptRow mg@(MurderGuess sus wea loc) (MurderGuess as aw al) = do
  let isCorrect = (sus == as, wea == aw, loc == al)
      (csus, cwea, cloc) = attemptCSSClasses mg isCorrect
  [whamlet|<tr>
            <td class=#{csus}>#{suspectName sus}
            <td class=#{cwea}>#{weaponVerb wea}
            <td class=#{cloc}>#{locationName loc}
          |]
  toWidget
    [lucius| td { font-size : 2rem;
                  border-radius: 5px;
                  padding: 5px;
                }
             .#{correctClass} { background-color: green; }
             .#{incorrectClass} { background-color: red; }
           |]

attemptTable :: [Entity MurderMystery] -> MurderGuess -> Widget
attemptTable mas corr = do
  let mgs = catMaybes $ map (\(Entity _ mm) -> murderMysteryGuess mm >>= decodeMG) mas
  [whamlet|<table>
            $forall mg <- mgs
              ^{attemptRow mg corr}
          |]
  toWidget
    [lucius|table { border-spacing: 5px;
                    border-collapse: separate;
                  }
           |]


hmsolution :: Handler (Maybe MurderGuess)
hmsolution = do
  solnEnt <- runDB $ selectFirst [MurderMysteryIsCorrect ==. True] []
  case solnEnt of
    Nothing -> return Nothing
    Just (Entity _ mm) -> return (murderMysteryGuess mm >>= decodeMG)


getMurderR :: Handler Html
getMurderR = do
  (widg, enctype) <- generateFormPost murderForm
  msolution <- hmsolution
  case msolution of
    Nothing -> error "There's no solution in the database! This is an error in the site."
    Just solution -> do
      maybeCurrentUserId <- maybeAuthId
      case maybeCurrentUserId of
        Nothing -> redirect HomeR
        Just uid -> do
          muser <- runDB $ getEntity uid
          case muser of
            Nothing -> error "Logged in as a nonexistent user? This is a bug in the site!"
            Just _ -> do
              let history = attemptTable [] solution
              defaultLayout $ do
                setTitle "Murder Mystery"
                $(widgetFile "murder")


postMurderR :: Handler Html
postMurderR = do
  ((res, widg), enctype) <- runFormPost murderForm
  msolution <- hmsolution
  case msolution of
    Nothing -> error "There's no solution in the database! This is an error in the site."
    Just solution -> do
      case res of
        FormMissing -> redirect MurderR
        FormFailure _ -> redirect MurderR
        FormSuccess mg -> do
          maybeCurrentUserId <- maybeAuthId
          case maybeCurrentUserId of
            Just uid -> do
              now <- liftIO getCurrentTime
              let corr = mg == solution
                  guess = MurderMystery { murderMysteryUserId = uid
                                    , murderMysterySubmittedAt = Just now
                                    , murderMysteryGuess = Just $ encodeMG mg
                                    , murderMysteryIsCorrect = corr
                                    }
              _ <- runDB $ insertEntity guess
              let history = attemptTable [] solution
              defaultLayout $ do
                setTitle "Murder Mystery"
                $(widgetFile "murder")
            Nothing -> redirect HomeR
