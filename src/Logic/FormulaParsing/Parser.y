{
module Logic.FormulaParsing.Parser (Hap, happyFormula) where

import Data.Text (pack)

import Logic.FormulaParsing.Lexer (Token(..))
import Logic.Formulas
}

%name happyFormula fmla
%tokentype { Token }
%error { parseError }
%monad { Hap } { (>>=) } { return }

%token
    cj  { TokConj }
    dj  { TokDisj }
    im  { TokImpl }
    ng  { TokNeg }
    fm  { TokFum }
    '(' { TokLPar }
    ')' { TokRPar }
    atm { TokAtom $$ }

%nonassoc cj dj im
%left ng

%%

fmla
  : atm            { A . At . pack $ $1 }
  | fmla cj fmla   { B Conjunction $1 $3 }
  | fmla dj fmla   { B Disjunction $1 $3 }
  | fmla im fmla   { B Implication $1 $3 }
  | ng fmla        { U Negation $2 }
  | fm             { N Falsum }
  | '(' fmla ')'   { $2 }

{
parseError :: [Token] -> Hap a
parseError [] = Left UnexpectedEnd
parseError (tok:_) = Left (UnexpectedToken tok)

data HappyError
  = UnexpectedEnd
  | UnexpectedToken Token
  deriving (Show, Eq)

type Hap a = Either HappyError a


}
