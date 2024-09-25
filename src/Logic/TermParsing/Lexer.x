{
module Logic.TermParsing.Lexer (alexScanTokens, Token(..)) where
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
}
