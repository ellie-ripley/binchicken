{-# LANGUAGE OverloadedStrings #-}

module Logic.Lambda where

import Data.Text (Text)

data Term = Term

data TermError = TermError

displayTermError :: TermError -> Text
displayTermError TermError = "You done messed up!"

parseTerm :: Text -> Either TermError Term
parseTerm _ = Right Term
