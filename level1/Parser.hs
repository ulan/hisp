module Parser (parseHisp) where
import Expr
import Text.ParserCombinators.Parsec

mkPos x = Pos (sourceLine x) (sourceColumn x)

whitespace :: Parser [Char] 
whitespace = many (space <|> cmt)
  where cmt = char ';' >> many (noneOf "\n") >> many1 (char '\n') >> return ' '

expr :: Parser Expr
expr = do
  x <- quote <|> list <|> atom
  whitespace
  return x
  where atom = do spos <- getPosition >>= return.mkPos
                  name <- many1 (noneOf "() \n\t;")
                  return (A name spos)
        quote = do spos <- getPosition >>= return.mkPos
                   char '\''
                   x <- expr
                   return (L [A "quote" spos, x] spos)
        list = do spos <- getPosition >>= return.mkPos
                  char '('
                  exprs <- many (whitespace >> expr)
                  char ')'
                  return $ L exprs spos

hisp :: Parser [Expr]
hisp = many (whitespace >> expr)

parseHisp = parse hisp ""
