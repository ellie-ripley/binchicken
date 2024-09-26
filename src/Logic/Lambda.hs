{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.Lambda where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (nub, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Logic.TermParsing.Lexer (alexScanTokens, displayToken)
import Logic.TermParsing.Parser (happyTerm, Hap, HappyError(..))

import Logic.Lambdas.Types


-- SECTION: displaying terms and errors, parsing terms

displayLambda :: Text
displayLambda = "λ"

displayLVar :: LVar -> Text
displayLVar (LVar v) = v

pars :: Text -> Text
pars tx
  | T.length tx == 1 = tx
  | otherwise = "(" <> tx <> ")"


displayTermAllPars :: Term -> Text
displayTermAllPars (TVar lvr)    = displayLVar lvr
displayTermAllPars (TApp tl tr)  = pars (displayTermAllPars tl) <> pars (displayTermAllPars tr)
displayTermAllPars (TLam lvr tm) =
  displayLambda <> displayLVar lvr <> "." <> pars (displayTermAllPars tm)


displayTermMinPars :: Term -> Text
displayTermMinPars (TVar lvr) = displayLVar lvr
displayTermMinPars (TLam lvr tm) =
  displayLambda <> displayLVar lvr <> "." <> displayTermMinPars tm
displayTermMinPars (TApp tl@(TLam _ _) tr@(TApp _ _)) =
  pars (displayTermMinPars tl) <> pars (displayTermMinPars tr)
displayTermMinPars (TApp tl@(TLam _ _) tr) =
  pars (displayTermMinPars tl) <> displayTermMinPars tr
displayTermMinPars (TApp tl tr@(TApp _ _)) =
  displayTermMinPars tl <> pars (displayTermMinPars tr)
displayTermMinPars (TApp tl tr) =
  displayTermMinPars tl <> displayTermMinPars tr

displayTermError :: HappyError -> Text
displayTermError = \case
  UnexpectedEnd -> "Ended too soon!"
  UnexpectedToken tk -> "There's a " <> displayToken tk <> " where I can't make sense of it!"

displayDeBruijn :: DeBruijn -> Text
displayDeBruijn (FreeVar lvr) = displayLVar lvr
displayDeBruijn (BoundVar n)  = T.pack $ show n
displayDeBruijn (DBApp dl dr) = pars (displayDeBruijn dl) <> pars (displayDeBruijn dr)
displayDeBruijn (DBLam db)    = displayLambda <> pars (displayDeBruijn db)

parseTerm :: Text -> Hap Term
parseTerm = happyTerm . alexScanTokens . T.unpack


-- SECTION: checking alpha equivalence

freeVars :: Term -> [LVar]
freeVars = nub . (go [])
  where
    go :: [LVar] -> Term -> [LVar]
    go bound (TVar lvr)
      | lvr `elem` bound = []
      | otherwise        = [lvr]
    go bound (TApp tl tr)  = go bound tl <> go bound tr
    go bound (TLam lvr tm) = go (lvr : bound) tm

dbFreeVars :: DeBruijn -> [LVar]
dbFreeVars = nub . go
  where
    go :: DeBruijn -> [LVar]
    go (FreeVar lvr) = [lvr]
    go (BoundVar _)  = []
    go (DBApp dl dr) = go dl <> go dr
    go (DBLam db)    = go db

data DeBruijn
  = FreeVar LVar
  | BoundVar Int
  | DBApp DeBruijn DeBruijn
  | DBLam DeBruijn
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

dbBind :: LVar -> DeBruijn -> DeBruijn
dbBind lvr old = DBLam (bump 0 old)
  where
    bump :: Int -> DeBruijn -> DeBruijn
    bump n fv@(FreeVar lv)
      | lv == lvr = BoundVar n
      | otherwise = fv
    bump _ bv@(BoundVar _) = bv
    bump n (DBApp dl dr)   = DBApp (bump n dl) (bump n dr)
    bump n (DBLam db)      = DBLam $ bump (n + 1) db

deBruijn :: Term -> DeBruijn
deBruijn (TVar lvr) = FreeVar lvr
deBruijn (TApp tl tr) = DBApp (deBruijn tl) (deBruijn tr)
deBruijn (TLam lvr tm) = dbBind lvr (deBruijn tm)

odds :: [a] -> [a]
odds []  = []
odds (_:xs) = evens xs

evens :: [a] -> [a]
evens []  = []
evens [x] = [x]
evens (x:xs) = x : odds xs

namify :: [LVar] -> DeBruijn -> Maybe Term
namify lvrs db =
  let freshVars = lvrs \\ dbFreeVars db
  in  go Map.empty freshVars db
         where
           dbIncrement :: Map Int LVar -> LVar -> Map Int LVar
           dbIncrement mp lv = Map.insert 0 lv (bump mp)

           bump :: Map Int LVar -> Map Int LVar
           bump = Map.fromList . (map (\(x, v) -> (x + 1, v))) . Map.toList

           go :: Map Int LVar -> [LVar] -> DeBruijn -> Maybe Term
           go _  _ (FreeVar lvr) = Just $ TVar lvr
           go mp _ (BoundVar n)  = do
             lv <- Map.lookup n mp
             return (TVar lv)
           go mp fv (DBApp dl dr) = do
             tl <- go mp (evens fv) dl
             tr <- go mp (odds fv)  dr
             return (TApp tl tr)
           go mp (v:vs) (DBLam dbb) = do
             tm <- go (dbIncrement mp v) vs dbb
             return (TLam v tm)
           go _ [] (DBLam _) = Nothing

data IsRedex a
  = NotARedex
  | RedexReduced a
  deriving (Eq, Show)

displayDBIsRedex :: IsRedex DeBruijn -> Text
displayDBIsRedex NotARedex = "Not a redex"
displayDBIsRedex (RedexReduced db) = "Is a redex; reduces to " <> displayDeBruijn db

dbIsRedex :: DeBruijn -> IsRedex DeBruijn
dbIsRedex (DBApp (DBLam body) arg) = RedexReduced (sub 0 body arg)
  where
    sub :: Int -> DeBruijn -> DeBruijn -> DeBruijn
    sub _ fv@(FreeVar _) _ = fv
    sub n bv@(BoundVar m) ar
      | m == n = ar
      | otherwise = bv
    sub n (DBApp dl dr) ar = DBApp (sub n dl ar) (sub n dr ar)
    sub n (DBLam db) ar = DBLam (sub (n + 1) db ar)
dbIsRedex _ = NotARedex

data ReductionResult a
  = Normal a
  | Reduced a
  deriving (Eq, Show, Functor)

displayDBReductionResult :: ReductionResult DeBruijn -> Text
displayDBReductionResult (Normal db) = (displayDeBruijn db) <> " is normal"
displayDBReductionResult (Reduced db) = "Reduces to " <> (displayDeBruijn db)

instance Applicative ReductionResult where
  pure f = Normal f
  (Normal f) <*> (Normal x)   = Normal (f x)
  (Normal f) <*> (Reduced x)  = Reduced (f x)
  (Reduced f) <*> (Normal x)  = Reduced (f x)
  (Reduced f) <*> (Reduced x) = Reduced (f x)

-- | Does one step of reduction at each outermost redex, in parallel
dbParallelOneStep :: DeBruijn -> ReductionResult DeBruijn
dbParallelOneStep db =
  case db of
    fv@(FreeVar _)   -> Normal fv
    bv@(BoundVar _)  -> Normal bv
    (DBLam dbody)    -> DBLam <$> dbParallelOneStep dbody
    da@(DBApp dl dr) ->
      case dbIsRedex da of
        RedexReduced a -> Reduced a
        NotARedex -> DBApp <$> dbParallelOneStep dl <*> dbParallelOneStep dr

normaliseDB :: DeBruijn -> DeBruijn
normaliseDB db =
  case dbParallelOneStep db of
    Normal d -> d
    Reduced d -> normaliseDB d
