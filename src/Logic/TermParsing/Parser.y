{
module Logic.TermParsing.Parser (Hap, happyTerm) where

import Data.Text (pack)
import Logic.TermParsing.Lexer (Token(..))
import Logic.Lambdas.Types
}

%name happyTerm trm
%tokentype { Token }
%error { parseError }
%monad { Hap } { (>>=) } { return }

%token
  lm  { TokLambda }
  pr  { TokPeriod }
  '(' { TokLPar }
  ')' { TokRPar }
  vr  { TokVar $$ }

%%

trm
  : vr           { TVar . LVar . pack $ $1 }
  | '(' trm ')'  { $2 }
  | trm trm      { TApp $1 $2 }
  | lm vr pr trm { TLam (LVar . pack $ $2) $4 }

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
