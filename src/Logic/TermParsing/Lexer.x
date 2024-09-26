{
{-# LANGUAGE OverloadedStrings #-}
module Logic.TermParsing.Lexer (alexScanTokens, displayToken, Token(..)) where

import Data.Text (Text)
import qualified Data.Text as T
}

%wrapper "basic"

$varble = a-z

tokens :-

  $white+       ;
  \\            { \_ -> TokLambda }
  λ             { \_ -> TokLambda }
  \.            { \_ -> TokPeriod }
  \(            { \_ -> TokLPar }
  \)            { \_ -> TokRPar }
  $varble       { \v -> TokVar v }

{
data Token
  = TokVar String
  | TokLambda
  | TokPeriod
  | TokLPar
  | TokRPar
  deriving (Eq, Show)

displayToken :: Token -> Text
displayToken (TokVar s) = "variable " <> T.pack s
displayToken TokLambda  = "λ"
displayToken TokPeriod  = "."
displayToken TokLPar    = "("
displayToken TokRPar    = ")"
}
