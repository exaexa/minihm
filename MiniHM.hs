{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| A simple implementation of Hindley-Milner system that prints out the
 - inference steps. -}
module MiniHM where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (isAlpha, isAlphaNum)
import Data.List (nub)
import Prelude hiding (abs)
import Text.ParserCombinators.ReadP

-- | Type of identifiers (we now work with `String`s but that might change).
type Id = String

-- | Type of lambda terms, extended with the polymorphic @let@ construction.
data Lam
  = App Lam Lam
  | Abs Id Lam
  | Var Id
  | Let Id Lam Lam

-- | Show the `Lam` type. Technically, this instance isn't following the usual
-- rules for `Show`, but we do that mainly for simplicity here.
instance Show Lam where
  show (Var v) = v
  show (Abs v l) = "\\" ++ v ++ " . " ++ show l
  show (Let v d e) = "let " ++ v ++ " = " ++ show d ++ " in " ++ show e
  show (App l r) = showFun l ++ " " ++ showParam r
    where
      showFun (Var v) = v
      showFun a@(App _ _) = show a
      showFun x = "(" ++ show x ++ ")"
      showParam (Var v) = v
      showParam x = "(" ++ show x ++ ")"

-- | Type of types in simply-typed lambda calculus (STLC), with type variables
-- marked by the parameter @a@.
data Ty a
  = TyVar
      { tyVarId :: a
      } -- ^ Type variable
  | TyFun (Ty a) (Ty a) -- ^ Type function, with fields for "parameter" and "result".

-- | As with `Lam`, the instance for showing `Ty` is not following the usual
-- rules, mainly for simplicity.
instance Show a => Show (Ty a) where
  show (TyVar i) = show i
  show (TyFun l@(TyFun _ _) r) = "(" ++ show l ++ ") -> " ++ show r
  show (TyFun l r) = show l ++ " -> " ++ show r

-- | `Ty` is a functor in its type argument.
instance Functor Ty where
  fmap f (TyVar a) = TyVar (f a)
  fmap f (TyFun l r) = TyFun (fmap f l) (fmap f r)

-- | Type of Hindley-Milner types; essentially STLC extended by the "forall"
-- prefix.
data PolyTy a
  = Mono (Ty a)
  | Forall a (PolyTy a)

-- | Shows a `PolyTy`. Again, not following the parseability rule.
instance Show a => Show (PolyTy a) where
  show (Mono t) = show t
  show (Forall a t) = "forall " ++ show a ++ " . " ++ show t

-- | `PolyTy` is a functor (basically by deferring to to `Ty`).
instance Functor PolyTy where
  fmap f (Mono t) = Mono (fmap f t)
  fmap f (Forall a t) = Forall (f a) (fmap f t)

-- | Retrieve a list of all type variables that occur in a type. The variables
-- may repeat.
allTyVars :: Ty a -> [a]
allTyVars (TyVar v) = [v]
allTyVars (TyFun l r) = allTyVars l ++ allTyVars r

-- | Like `allTyVars` but for a `PolyTy`.
allPolyTyVars :: PolyTy a -> [a]
allPolyTyVars (Mono t) = allTyVars t
allPolyTyVars (Forall a t) = [a] ++ allPolyTyVars t

-- | Re-number the variables in a given type so that the variable numbers start
-- with 0 and all `TyVar` numbers are used continuously all the way to some
-- given maximum.
squashVars :: (Functor f) => (f Int -> [Int]) -> f Int -> f Int
squashVars f t =
  let dict = zip (nub $ f t) [0 ..]
   in maybe undefined id . flip lookup dict <$> t

-- | Type of parsers.
type Parser = ReadP

-- | Run a parser. Defers to `readP_to_S`.
runParser :: ReadP a -> ReadS a
runParser = readP_to_S

-- | Run a given parser, and also consume all spaces that follow it.
lexeme :: Parser a -> Parser a
lexeme = (<* skipSpaces)

-- | Parse out a single-character lexeme.
lchar :: Char -> Parser ()
lchar = void . lexeme . char

-- | Parse out an identifier lexeme.
someId :: Parser Id
someId = lexeme $ (:) <$> satisfy isAlpha <*> munch isAlphaNum

-- | Parse out an identifier which is not a keyword (i.e., it doesn't conflict
-- with the H-M syntax, and thus may serve as a variable name.)
varId :: Parser Id
varId = do
  v <- someId
  if v `elem` ["in", "let"]
    then fail "lone keyword"
    else pure v

-- | Parse out a parenthesized something.
parens :: Parser Lam
parens = between (lchar '(') (lchar ')') lam

-- | Parse a lambda abstraction.
abs :: Parser Lam
abs = do
  lchar '\\'
  v <- varId
  lchar '.'
  t <- lam
  pure $ Abs v t

-- | Parse the let-in formula.
letin :: Parser Lam
letin = do
  "let" <- someId
  v <- varId
  lchar '='
  d <- lam
  "in" <- someId
  e <- lam
  pure $ Let v d e

-- | Parse an "atomic" simple lambda term.
atomLam :: Parser Lam
atomLam = (Var <$> varId) <|> parens

-- | Parse a chain of `atomLam`s and fold them from left with `App`, to form a
-- long application.
app :: Parser Lam
app = foldl1 App <$> many1 atomLam

-- | Parse out a lambda term.
lam :: Parser Lam
lam = letin <|> abs <|> app

-- | Like `lam` but skips initial spaces and checks if it parsed to the end of
-- the input.
lambda :: Parser Lam
lambda = skipSpaces *> lam <* eof

-- | Type inference context; stores the "state" of the inference algorithm and
-- some extra methods.
data Ctxt m = Ctxt
  { subst :: [(Int, Ty Int)] -- | all substitution equations
  , vars :: [(Id, PolyTy Int)] -- | context ("base") with all variable types
  , next :: Int -- | ID of the next unused ("fresh") type variable
  , nest :: Int -- | how deeply we're nested (used for printing the output)
  , putLine :: String -> m () -- | a function to write an inference step out
  , throwError :: forall a. String -> m a -- | a function to terminate with an error
  }

-- | Default "empty" starting context (initialized with methods).
emptyCtxt :: (String -> m ()) -> (forall a. String -> m a) -> Ctxt m
emptyCtxt pl e =
  Ctxt {subst = [], vars = [], next = 0, nest = 0, putLine = pl, throwError = e}

-- | Type inference monad. We keep a stateful `Ctxt` atop of IO.
type Infer m = StateT (Ctxt m) m

-- | Print out an info message from the inference.
info :: Monad m => String -> Infer m ()
info i = do
  indent <- gets nest
  p <- gets putLine
  lift . p $ replicate (2 * indent) ' ' ++ "- " ++ i

-- | Terminate inference with an error.
explode ::
     Monad m
  => String
  -> Infer m a
explode e = do
  c <- Control.Monad.Trans.State.get
  lift (throwError c e)

-- | Run a "nested" part of the inference, changing the output to look like a
-- deeper nesting of a list.
nested :: Monad m => Infer m a -> Infer m a
nested x = do
  n <- gets nest
  modify $ \s -> s {nest = succ n}
  res <- x
  modify $ \s -> s {nest = n}
  pure res

-- | Check if a variable occurs in a type.
occurs :: Int -> Ty Int -> Bool
occurs v (TyVar w) = v == w
occurs v (TyFun l r) = occurs v l || occurs v r

-- | Unify 2 type terms, producing new substitutions where required. The input
-- terms must be fully substitued.
unify :: Monad m => Ty Int -> Ty Int -> Infer m ()
unify (TyFun l1 r1) (TyFun l2 r2) = do
  unify l1 l2
  (,) <$> fullSubst r1 <*> fullSubst r2 >>= uncurry unify
unify (TyVar v1) t2@(TyVar v2)
  | v1 == v2 = pure ()
  | otherwise = assign v1 t2
unify t1 (TyVar v2) = assign v2 t1
unify (TyVar v1) t2 = assign v1 t2

-- | Unify 2 type terms, and make sure they are fully substitued before they
-- hit `unify`.
unify' :: Monad m => Ty Int -> Ty Int -> Infer m ()
unify' x' y' = do
  x <- fullSubst x'
  y <- fullSubst y'
  info $ "solving unification (" ++ showGreek x ++ ") = (" ++ showGreek y ++ ")"
  unify x y

-- | Add a new substitution (v := t') to the context.
assign :: Monad m => Int -> Ty Int -> Infer m ()
assign v t' = do
  t <- fullSubst t'
  if occurs v t
    then explode
           $ "occurs check: can't substitute for "
               ++ show (greekLabels !! v)
               ++ " into type "
               ++ showGreek t
    else do
      info $ "substitute " ++ show (greekLabels !! v) ++ " := " ++ showGreek t
      modify $ \s -> s {subst = (v, t) : subst s}

-- | Fully apply the current substitution to the given term.
fullSubst :: Monad m => Ty Int -> Infer m (Ty Int)
fullSubst (TyFun l r) = TyFun <$> fullSubst l <*> fullSubst r
fullSubst tv@(TyVar v) = do
  s <- lookup v <$> gets subst
  case s of
    Nothing -> pure tv
    Just t -> fullSubst t

-- | Create a type variable which isn't used anywhere else (because it's
-- getting a fresh "counter" value).
fresh :: Monad m => Infer m (Ty Int)
fresh = do
  v <- gets next
  modify $ \s -> s {next = succ v}
  pure $ TyVar v

-- | Generalize a type by labeling all free variables with a "forall".
generalize :: Ty Int -> PolyTy Int
generalize t' =
  let t = t' --note: we might have `squashVars allTyVars t'` here, but it obscures where the variables came from
      vs = nub (allTyVars t)
   in foldr Forall (Mono t) vs

-- | Instantiate a generalized type by using fresh type variables instead of
-- all forall-ed ones.
instantiate :: Monad m => PolyTy Int -> Infer m (Ty Int)
instantiate = go []
  where
    go s (Mono t) = pure $ maybe undefined id . flip lookup s <$> t
    go s (Forall v t) = do
      x <- tyVarId <$> fresh
      go ((v, x) : s) t

-- | Do the inference on a given term.
infer :: Monad m => Lam -> Infer m (Ty Int)
infer x@(Var v) = do
  info $ "inferring " ++ show x
  t <- lookup v <$> gets vars
  case t of
    Just (Mono t') -> do
      res <- fullSubst t'
      info $ "found monomorphic " ++ v ++ " :: " ++ showGreek res
      pure res
    Just t' -> do
      info $ "found polymorphic " ++ v ++ " :: " ++ showGreek t'
      res <- instantiate t'
      info $ "instantiated " ++ v ++ " :: " ++ showGreek res
      pure res
    _ -> explode $ "variable not in context: " ++ v
infer x@(App l r) = do
  info $ "inferring " ++ show x
  lt <- nested (infer l)
  rt <- nested (infer r) >>= fullSubst
  res <- fresh
  info $ "assuming " ++ show x ++ " to return " ++ showGreek res
  fullSubst lt >>= unify' (TyFun rt res)
  res' <- fullSubst res
  lt' <- fullSubst lt
  info $ "inferred " ++ show l ++ " :: " ++ showGreek lt'
  info $ "inferred " ++ show x ++ " :: " ++ showGreek res'
  pure res'
infer x@(Abs v l) = do
  info $ "inferring " ++ show x
  vt <- fresh
  vars' <- gets vars
  modify $ \s -> s {vars = (v, Mono vt) : vars'}
  info $ "pushed variable " ++ v ++ " into context with type " ++ showGreek vt
  t <- nested (infer l)
  vt' <- fullSubst vt
  modify $ \s -> s {vars = vars'}
  info $ "popped variable " ++ v ++ " from context with type " ++ showGreek vt'
  res <- fullSubst $ TyFun vt t
  info $ "inferred " ++ show x ++ " :: " ++ showGreek res
  pure res
infer x@(Let v d e) = do
  info $ "inferring " ++ show x
  vars' <- gets vars
  dtr <- fresh
  modify $ \s -> s {vars = (v, Mono dtr) : vars'}
  info
    $ "pushed rec-variable " ++ v ++ " into context with type " ++ showGreek dtr
  dt <- nested (infer d) >>= fullSubst
  fullSubst dtr >>= unify' dt
  dt' <- generalize <$> fullSubst dt
  modify $ \s -> s {vars = (v, dt') : vars'}
  info $ "generalized let-variable " ++ v ++ " to type " ++ showGreek dt'
  et <- nested (infer e)
  modify $ \s -> s {vars = vars'}
  info $ "popped let-variable " ++ v ++ " from context"
  res <- fullSubst et
  info $ "inferred " ++ show x ++ " :: " ++ showGreek res
  pure res

-- | A newtype-wrapper for `String` labels of variables.
newtype Label =
  Label String

-- | Showing a given label just returns the contained string, without adding
-- quotes or other such things.
instance Show Label where
  show (Label s) = s

-- | Given a finite list of variable labels, make an infinite list of variable
-- labels (by adding numbers).
rangeLabels :: [Char] -> [Label]
rangeLabels range =
  [Label (x : y) | y <- ("" : map show [1 :: Int ..]), x <- range]

-- | Ascii-based labels for type variables
asciiLabels :: [Label]
asciiLabels = rangeLabels ['a' .. 'z']

-- | Greek labels for type variables (used during inference)
greekLabels :: [Label]
greekLabels = rangeLabels ['α' .. 'ω']

-- | Show the type with greek variable labels.
showGreek :: (Functor t, Show (t Label)) => t Int -> String
showGreek = show . fmap (greekLabels !!)

-- | Show a type nicely, with variable renumbering, with the given labels.
niceType :: [Label] -> Ty Int -> Ty Label
niceType labels t = (labels !!) <$> squashVars allTyVars t

-- | Run the type inference on a lambda term in a string, and print the result.
inferType ::
     Monad m => String -> (String -> m ()) -> (forall a. String -> m a) -> m ()
inferType str out err =
  case runParser lambda str of
    [(l, _)] -> do
      (t, _) <- infer l `runStateT` emptyCtxt out err
      out $ ">>> " ++ show l ++ "\n :: " ++ show (niceType asciiLabels t)
    x -> err $ "parsing failed or ambiguous: " ++ show x
