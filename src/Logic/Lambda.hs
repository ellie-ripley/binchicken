{-# LANGUAGE OverloadedStrings #-}

module Logic.Lambda where

import Data.Text (Text)
import qualified Data.Text as T

import Logic.TermParsing.Lexer (alexScanTokens)
import Logic.TermParsing.Parser (happyTerm)

import Logic.Lambdas.Types


data TermError = TermError

displayLambda :: Text
displayLambda = "λ"

displayLVar :: LVar -> Text
displayLVar (LVar v) = v

pars :: Text -> Text
pars tx = "(" <> tx <> ")"

displayTermAllPars :: Term -> Text
displayTermAllPars (TVar lvr)    = displayLVar lvr
displayTermAllPars (TApp tl tr)  = pars (displayTermAllPars tl) <> pars (displayTermAllPars tr)
displayTermAllPars (TLam lvr tm) =
  displayLambda <> displayLVar lvr <> "." <> pars (displayTermAllPars tm)

displayTermError :: TermError -> Text
displayTermError TermError = "You done messed up!"

parseTerm :: Text -> Either TermError Term
parseTerm tx =
  case happyTerm . alexScanTokens . T.unpack $ tx of
    Left _   -> Left TermError
    Right tm -> Right tm
