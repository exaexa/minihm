module Main where

import Control.Applicative
import Control.Exception
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (isAlpha, isSpace)
import Data.List (nub)
import Prelude hiding (abs)
import System.Console.Haskeline
import Text.ParserCombinators.ReadP

{-
 - types of terms
 -}
type Id = String

data Lam
  = App Lam Lam
  | Abs Id Lam
  | Var Id
  | Let Id Lam Lam

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

{-
 - types of types
 -}
data Ty a
  = TyVar
      { tyVarId :: a
      }
  | TyFun (Ty a) (Ty a)

instance Show a => Show (Ty a) where
  show (TyVar i) = show i
  show (TyFun l@(TyFun _ _) r) = "(" ++ show l ++ ") -> " ++ show r
  show (TyFun l r) = show l ++ " -> " ++ show r

instance Functor Ty where
  fmap f (TyVar a) = TyVar (f a)
  fmap f (TyFun l r) = TyFun (fmap f l) (fmap f r)

data PolyTy a
  = Mono (Ty a)
  | Forall a (PolyTy a)

instance Show a => Show (PolyTy a) where
  show (Mono t) = show t
  show (Forall a t) = "forall " ++ show a ++ " . " ++ show t

instance Functor PolyTy where
  fmap f (Mono t) = Mono (fmap f t)
  fmap f (Forall a t) = Forall (f a) (fmap f t)

allTyVars :: Ty a -> [a]
allTyVars (TyVar v) = [v]
allTyVars (TyFun l r) = allTyVars l ++ allTyVars r

allPolyTyVars :: PolyTy a -> [a]
allPolyTyVars (Mono t) = allTyVars t
allPolyTyVars (Forall a t) = [a] ++ allPolyTyVars t

squashVars :: (Functor f) => (f Int -> [Int]) -> f Int -> f Int
squashVars f t =
  let dict = zip (nub $ f t) [0 ..]
   in maybe undefined id . flip lookup dict <$> t

{-
 - parser
 -}
type Parser = ReadP

runParser :: ReadP a -> ReadS a
runParser = readP_to_S

lexeme :: Parser a -> Parser a
lexeme = (<* skipSpaces)

lchar :: Char -> Parser ()
lchar = void . lexeme . char

someId :: Parser String
someId = lexeme $ munch1 isAlpha

varId :: Parser String
varId = do
  v <- someId
  if v `elem` ["in", "let"]
    then fail "lone keyword"
    else pure v

parens :: Parser Lam
parens = between (lchar '(') (lchar ')') lam

abs :: Parser Lam
abs = do
  lchar '\\'
  v <- varId
  lchar '.'
  t <- lam
  pure $ Abs v t

letin :: Parser Lam
letin = do
  "let" <- someId
  v <- varId
  lchar '='
  d <- lam
  "in" <- someId
  e <- lam
  pure $ Let v d e

atomLam :: Parser Lam
atomLam = (Var <$> varId) <|> parens

app :: Parser Lam
app = foldl1 App <$> many1 atomLam

lam :: Parser Lam
lam = letin <|> abs <|> app

lambda :: Parser Lam
lambda = skipSpaces *> lam <* eof

{-
 - type system helpers
 -}
data Ctxt = Ctxt
  { subst :: [(Int, Ty Int)] -- | all substitution equations
  , vars :: [(Id, PolyTy Int)] -- | context ("base") with all variable types
  , next :: Int -- | ID of the next unused ("fresh") type variable
  , nest :: Int -- | how much deeply we're nested
  } deriving (Show)

info :: String -> Infer ()
info i = do
  indent <- gets nest
  lift . putStrLn $ replicate (2 * indent) ' ' ++ "- " ++ i

nested :: Infer a -> Infer a
nested x = do
  n <- gets nest
  modify $ \s -> s {nest = succ n}
  res <- x
  modify $ \s -> s {nest = n}
  pure res

type Infer = StateT Ctxt IO

emptyCtxt :: Ctxt
emptyCtxt = Ctxt {subst = [], vars = [], next = 0, nest = 0}

-- check if a variable occurs in a type
occurs :: Int -> Ty Int -> Bool
occurs v (TyVar w) = v == w
occurs v (TyFun l r) = occurs v l || occurs v r

-- unify 2 type terms, producing new substitutions where required
unify :: Ty Int -> Ty Int -> Infer ()
unify (TyFun l1 r1) (TyFun l2 r2) = do
  unify l1 l2
  (,) <$> fullSubst r1 <*> fullSubst r2 >>= uncurry unify
unify (TyVar v1) t2@(TyVar v2)
  | v1 == v2 = pure ()
  | otherwise = assign v1 t2
unify t1 (TyVar v2) = assign v2 t1
unify (TyVar v1) t2 = assign v1 t2

-- make a new substitution (v := t)
assign :: Int -> Ty Int -> Infer ()
assign v t' = do
  t <- fullSubst t'
  if occurs v t
    then fail
           $ "occurs check: can't substitute for "
               ++ (greekLabels !! v)
               ++ " into type: "
               ++ showGreek t
    else do
      info $ "substitute: " ++ (greekLabels !! v) ++ " := " ++ showGreek t
      modify $ \s -> s {subst = (v, t) : subst s}

-- fully apply the current substitution
fullSubst :: Ty Int -> Infer (Ty Int)
fullSubst (TyFun l r) = TyFun <$> fullSubst l <*> fullSubst r
fullSubst tv@(TyVar v) = do
  s <- lookup v <$> gets subst
  case s of
    Nothing -> pure tv
    Just t -> fullSubst t

-- create a variable which isn't used anywhere else
fresh :: Infer (Ty Int)
fresh = do
  v <- gets next
  modify $ \s -> s {next = succ v}
  pure $ TyVar v

-- let-polymorphism-handling functions
generalize :: Ty Int -> PolyTy Int
generalize t' =
  let t = squashVars allTyVars t'
      vs = nub (allTyVars t)
   in foldr Forall (Mono t) vs

instantiate :: PolyTy Int -> Infer (Ty Int)
instantiate = go []
  where
    go s (Mono t) = pure $ maybe undefined id . flip lookup s <$> t
    go s (Forall v t) = do
      x <- tyVarId <$> fresh
      go ((v, x) : s) t

{-
 - the actual typesystem
 -}
infer :: Lam -> Infer (Ty Int)
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
    _ -> fail $ "variable not in context: " ++ v
infer x@(App l r) = do
  info $ "inferring " ++ show x
  lt <- nested (infer l)
  rt <- nested (infer r) >>= fullSubst
  res <- fresh
  info $ "assuming " ++ show x ++ " to return " ++ showGreek res
  fullSubst lt >>= unify (TyFun rt res)
  res' <- fullSubst res
  lt' <- fullSubst lt
  info $ "inferred: " ++ show l ++ " :: " ++ showGreek lt'
  info $ "inferred: " ++ show x ++ " :: " ++ showGreek res'
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
  info $ "inferred: " ++ show x ++ " :: " ++ showGreek res
  pure res
infer x@(Let v d e) = do
  info $ "inferring " ++ show x
  vars' <- gets vars
  dtr <- fresh
  modify $ \s -> s {vars = (v, Mono dtr) : vars'}
  info
    $ "pushed rec-variable " ++ v ++ " into context with type " ++ showGreek dtr
  dt <- nested (infer d) >>= fullSubst
  fullSubst dtr >>= unify dt
  dt' <- generalize <$> fullSubst dt
  modify $ \s -> s {vars = (v, dt') : vars'}
  info $ "generalized let-variable " ++ v ++ " to type " ++ showGreek dt'
  et <- nested (infer e)
  modify $ \s -> s {vars = vars'}
  info $ "popped let-variable " ++ v ++ " from context"
  res <- fullSubst et
  info $ "inferred: " ++ show x ++ " :: " ++ showGreek res
  pure res

{-
 - nice printing of types
 -}
newtype Label =
  Label String

instance Show Label where
  show (Label s) = s

rangeLabels :: [Char] -> [String]
rangeLabels range = [x : y | y <- ("" : map show [1 :: Int ..]), x <- range]

asciiLabels :: [String]
asciiLabels = rangeLabels ['a' .. 'z']

greekLabels :: [String]
greekLabels = rangeLabels ['α' .. 'ω']

showGreek :: (Functor t, Show (t Label)) => t Int -> String
showGreek = show . fmap (Label . (greekLabels !!))

niceType :: [String] -> Ty Int -> Ty Label
niceType labels t = Label . (labels !!) <$> squashVars allTyVars t

inferType :: String -> IO ()
inferType str =
  case runParser lambda str of
    [(l, _)] -> do
      (t, _) <- runStateT (infer l) emptyCtxt
      putStrLn $ ">>> " ++ show l ++ "\n :: " ++ show (niceType asciiLabels t)
    x -> fail $ "parsing failed or ambiguous: " ++ show x

{-
 - user front-end
 -}
main :: IO ()
main = runInputT defaultSettings repl
  where
    repl = do
      l <- getInputLine "hm> "
      case l of
        Nothing -> pure ()
        Just s
          | all isSpace s -> repl
          | otherwise -> do
            lift $ inferType s `catch` \e -> print (e :: IOException)
            lift $ putStrLn ""
            repl
