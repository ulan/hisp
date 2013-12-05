module Main where
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Maybe
import Error
import Expr
import Parser
import qualified Data.Map as DM 
import System.Exit 
import Text.Printf

type Eval a = ErrorT EvalError Identity a

type Checker = [Expr] -> Pos -> Eval ()
type StrictHandler = Env -> [(Value, Pos)] -> Eval Value
type LazyHandler = Env -> [Expr] -> Eval Value
data Builtin = Strict Checker StrictHandler
             | Lazy Checker LazyHandler 

builtins = DM.fromList [
  ("atom",   (Strict (arguments 1)         atom)),
  ("car",    (Strict (arguments 1)         car)),
  ("cdr",    (Strict (arguments 1)         cdr)),
  ("cons",   (Strict (arguments 2)         cons)),
  ("cond",   (Lazy   (manyArgumentsFrom 1) cond)),
  ("eq",     (Strict (arguments 2)         eq)),
  ("label",  (Lazy   (arguments 2)         label)),
  ("lambda", (Lazy   (arguments 2)         lambda)),
  ("quote",  (Lazy   (arguments 1)         quote)),
  ("+",      (Strict (manyArgumentsFrom 2) add)),
  ("-",      (Strict (manyArgumentsFrom 2) sub)),
  ("*",      (Strict (manyArgumentsFrom 2) mul)),
  ("/",      (Strict (manyArgumentsFrom 2) divide))]

atom env [(Number  _, _)] = return $ Boolean True
atom env [(Boolean _, _)] = return $ Boolean True
atom env [(Symbol  _, _)] = return $ Boolean True
atom env [(List [], _)]   = return $ Boolean True
atom env _ = return $ Boolean False

car env [(list, p)] = do
  xs <- asList (list, p)
  when (xs == []) $ throwError $ ExpectListOfAtLeast 1 list p
  return $ head xs

cdr env [(list, p)] = do
  xs <- asList (list, p)
  when (xs == []) $ throwError $ ExpectListOfAtLeast 1 list p
  return $ List (tail xs)

cons env [(x, _), list] = do
  xs <- asList list
  return $ List (x:xs) 

cond env ((L [caseCond, caseBody] _):rest) = do
  caseCond' <- (eval env caseCond >>= \v -> (asBoolean (v, pos caseCond)))
  if caseCond'
    then eval env caseBody 
    else cond env rest
cond env (head:rest) =
  throwError $ ExpectListOf 2 (quoteEval head) (pos head) 

eq env [(a, _), (b, _)] | a == b    = return $ Boolean True 
                        | otherwise = return $ Boolean False

label env [atom, lambda] = do
  name <- asName atom
  closure <- eval (DM.delete name env) lambda
  (params, body, env) <- asClosure (closure, pos lambda)
  let env' = DM.insert name closure env
  return $ Closure params body env'  

lambda env [args, body] = do
  names <- asNames args
  return $ Closure names body env

quote env [expr] = return (quoteEval expr)

add env xs = do
  nums <- asNumbers xs
  return $ Number (sum nums)

sub env xs = do
  nums <- asNumbers xs
  return $ Number (head nums - (sum (tail nums)))

mul env xs = do
  nums <- asNumbers xs
  return $ Number (product nums)

divide env xs = do
  let zero = filter (\(x, p) -> x == Number 0) (tail xs)
  when (not $ zero /= []) $ throwError $ DivisionByZero (snd $ head zero)
  nums <- asNumbers xs
  let (a:bs) = nums
  let result = foldl (\x y -> x `div` y) a bs
  return $ Number result

asBoolean :: (Value, Pos) -> Eval Bool
asBoolean (Boolean x, _) = return x
asBoolean (x, p) = throwError $ ExpectBoolean x p

asString :: (Value, Pos) -> Eval String
asString (Symbol x, _) = return x
asString (x, p) = throwError $ ExpectSymbol x p

asNumber :: (Value, Pos) -> Eval Integer
asNumber (Number x, _) = return x
asNumber (x, p) = throwError $ ExpectNumber x p

asNumbers xs = mapM asNumber xs

asList :: (Value, Pos) -> Eval [Value]
asList (List x, p) = return x
asList (x, p) = throwError $ ExpectList x p

asClosure :: (Value, Pos) -> Eval ([String], Expr, Env)
asClosure (Closure args body env, _) = return (args, body, env)
asClosure (x, p) = throwError $ ExpectClosure x p

asName (A name _) = return name
asName x = throwError $ ExpectSymbol (quoteEval x) (pos x) 
asNames (L xs _) = mapM asName xs
asNames x = throwError $ ExpectList (quoteEval x) (pos x)

arguments n args p = 
  when (n /= length args) $ throwError $ ExpectArguments n (length args) p

manyArgumentsFrom n args p = 
  when (n > length args) $ throwError $ ExpectArgumentsAtLeast n (length args) p

quoteEval :: Expr -> Value
quoteEval (A x _) = Symbol x
quoteEval (L x _) = List (map quoteEval x)

eval :: Env -> Expr -> Eval Value
eval env (A atom _) | atom == "true" = return $ Boolean True
eval env (A atom _) | atom == "false" = return $ Boolean False
eval env (A atom _) | all isDigit atom = return $ Number (read atom :: Integer)
eval env (A atom p) =
  case (DM.lookup atom env) of
    Just value -> return value 
    Nothing    -> throwError $ UnboundIdentifier atom p
eval env (L (head@(A name _):tail) p) | name `DM.member` builtins = do
  case fromJust $ DM.lookup name builtins of
    Strict checker handler -> do
      checker tail p
      args <- mapM (eval env) tail
      handler env (zip args (map pos tail))
    Lazy checker handler -> do
      checker tail p
      handler env tail
eval env (L (head:tail) pos) = do
  (params, body, closureEnv) <- (eval env head >>= \v -> (asClosure (v, pos)))
  args <- mapM (eval env) tail
  manyArgumentsFrom (length params) tail pos
  let (named, rest) = splitAt (length params) args
  let argsEnv = DM.fromList ((zip params named) ++ [("arguments", List rest)])
  let env' = argsEnv `DM.union` closureEnv `DM.union` env
  eval env' body 

runEval env expr = do
  case runIdentity (runErrorT (eval env expr)) of
    Right val -> return val
    Left error -> do putStrLn ("While executing " ++ (show (pos expr)) ++ ":") 
                     putStrLn (show error)
                     exitFailure

isDefun :: Expr -> Bool
isDefun (L ((A "defun" _):_) _) = True
isDefun _ = False

defun :: Env -> Expr -> IO Env
defun env (L [A "defun" p, name@(A sname _), params, body] _) = do
  let lambda = L [(A "lambda" p), params, body] p
  let label = L [(A "label" p), name, lambda] p
  val <- runEval env lambda
  putStrLn (sname)
  return $ DM.insert sname val env

main = do str <- getContents
          case parseHisp str of
            Left err -> print err
            Right exprs -> do
              let (defuns, rest) = partition isDefun exprs
              env <- foldM defun DM.empty defuns
              mapM_ (\x -> runEval env x >>= (putStrLn . show)) rest
