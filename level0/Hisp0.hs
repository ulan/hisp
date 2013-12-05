module Main where
import Data.List
import Text.ParserCombinators.Parsec

data Expr = Atom String
          | List [Expr]
  deriving (Eq, Show)

mkTrue  = Atom "true"
mkFalse = Atom "false" 
isTrue (Atom "true") = True
isTrue otherwise  = False
isFalse (Atom "false") = True
isFalse otherwise = False
mkLambda params body = List [Atom "lambda", params, body]
         
eval env x | isTrue x = mkTrue 
eval env x | isFalse x = mkFalse 
eval env (Atom s) = env (Atom s)
eval env (List ((Atom x):args)) = case x of
  "quote"  -> quote args
  "lambda" -> lambda args
  "label"  -> label args
  "cond"   -> cond args
  "eq"     -> eq (map (eval env) args)
  "atom"   -> atom (map (eval env) args)
  "cons"   -> cons (map (eval env) args)
  "car"    -> car  (map (eval env) args)
  "cdr"    -> cdr  (map (eval env) args)
  _        -> eval env (List ((eval env (Atom x)):args))
  where
    quote [a] = a
    lambda [args, body] = mkLambda args body
    label [Atom name, fun@(List [Atom "lambda", args, body])] =
      mkLambda args (List [mkLambda (List [Atom name]) body, fun])
    cond ((List [p, e]):rest) | isTrue p  = eval env e
                              | isFalse p = cond rest
                              | otherwise = cond ((List [eval env p, e]):rest)
    eq [a, b] | a == b    = mkTrue
              | otherwise = mkFalse
    atom [Atom _]  = mkTrue
    atom [List []] = mkTrue
    atom _         = mkFalse
    cons [a, List b] = List (a:b)
    car [List (x:xs)] = x
    cdr [List (x:xs)] = List xs
eval env (List ((List ((Atom "lambda"):decl)):args)) = apply decl args
  where apply [params, body] args = eval env' body 
          where env' x = case lookup x (zip (list params) args') of
                            Just x -> x
                            _ -> env x
                list (Atom x)  = [Atom x]
                list (List xs) = xs 
                args' = map (eval env) args
eval env (List (operator:args)) = eval env (List ((eval env operator):args))

whitespace :: Parser [Char] 
whitespace = many (space <|> cmt)
  where cmt = char ';' >> many (noneOf "\n") >> many1 (char '\n') >> return ' '

expr :: Parser Expr
expr = (quote <|> list <|> atom) >>= (\x -> whitespace >> return x)
  where atom = (many1 (noneOf "() \n\t;")) >>= return . Atom
        quote = char '\'' >> (fmap (\x -> List [Atom "quote", x]) expr)
        list = do char '('
                  exprs <- many (whitespace >> expr)
                  char ')'
                  return $ List exprs

program :: Parser [Expr]
program = many (whitespace >> expr)

out (Atom s) = s
out (List xs) = "(" ++ (intercalate " " (map out xs)) ++ ")" 

cc (vals, env) (List [Atom "defun", name, params, body]) = (name:vals, env')
  where val' = eval env (List [(Atom "label"), name, mkLambda params body])
        env' x = if x == name then val' else env x
cc (values, env) expr = ((eval env expr):values, env)

main = do str <- getContents
          case parse program "" str of
              Left err -> print err
              Right exprs  -> mapM_ (putStrLn . out) $ reverse $ fst $ foldl cc ([], id) $ exprs
