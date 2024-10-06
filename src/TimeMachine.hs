{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TimeMachine where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text, pack)
import Database.Persist.TH ( derivePersistField )
import GHC.Generics (Generic)


data Logician
  = Euclid
  | LaddF
  | Luka
  | Plumwood
  deriving (Bounded, Enum, Eq, Read, Show, Generic, ToJSON, FromJSON)
derivePersistField "Logician"

displayLogician :: Logician -> Text
displayLogician = \case
  Euclid   -> "Euclid of Alexandria"
  LaddF    -> "Christine Ladd-Franklin"
  Luka     -> "Jan Łukasiewicz"
  Plumwood -> "Val Plumwood"

displayLogicianShort :: Logician -> Text
displayLogicianShort = \case
  Euclid   -> "Euclid"
  LaddF    -> "Ladd-Franklin"
  Luka     -> "Łukasiewicz"
  Plumwood -> "Plumwood"

type Coords = (Int, Int, Int, Int, Int)

displayCoords :: Coords -> Text
displayCoords (a, b, c, d, e) = foldMap (pack . show) [a, b, c, d, e]

readDig :: Char -> Int
readDig = read . (:[])

leftPad :: String -> String
leftPad s = replicate (5 - length s) '0' ++ s

coordsFromInt :: Int -> Maybe Coords
coordsFromInt n
  | n < 0     = Nothing
  | n > 99999 = Nothing
  | otherwise = let tx = leftPad (show n)
                in Just ( readDig $ tx !! 0
                        , readDig $ tx !! 1
                        , readDig $ tx !! 2
                        , readDig $ tx !! 3
                        , readDig $ tx !! 4
                        )

intFromCoords :: Coords -> Int
intFromCoords (a, b, c, d, e)
  = a * 10000
  + b * 1000
  + c * 100
  + d * 10
  + e

combo :: Logician -> Coords
combo = \case
  Euclid   -> (8, 1, 8, 2, 8)
  LaddF    -> (3, 8, 5, 5, 1)
  Luka     -> (9, 7, 8, 2, 6)
  Plumwood -> (4, 8, 6, 0, 8)
