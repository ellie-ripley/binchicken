{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Logic.Lambdas.Types where

import Data.Aeson ( FromJSON
                  , ToJSON
                  )
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data LVar = LVar Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Term =
    TVar LVar
  | TApp Term Term
  | TLam LVar Term
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

lvarList :: [Char] -> [LVar]
lvarList = map (LVar . pack . (:[]))
