{
module Logic.TermParsing.Parser (Hap, HappyError(..), happyTerm) where

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

appy
  : trm       { $1 }
  | appy trm  { TApp $1 $2 }

trm
  : vr            { TVar . LVar . pack $ $1 }
  | appy          { $1 }
  | '(' appy ')'  { $2 }
  | lm vr pr appy { TLam (LVar . pack $ $2) $4 }

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
