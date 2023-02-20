{
module Logic.FormulaParsing.Lexer (alexScanTokens, Token(..)) where
}

%wrapper "basic"

$atom = a-z

tokens :-

  $white+       ;
  \/\\          { \_ -> TokConj }
  \\\/          { \_ -> TokDisj }
  \->           { \_ -> TokImpl }
  \~            { \_ -> TokNeg }
  _\|_          { \_ -> TokFum }
  \(            { \_ -> TokLPar }
  \)            { \_ -> TokRPar }
  $atom         { \s -> TokAtom s }
  $printable    ;

{
data Token
  = TokAtom String
  | TokConj
  | TokDisj
  | TokImpl
  | TokNeg
  | TokFum
  | TokLPar
  | TokRPar
  deriving (Eq, Show)


}
