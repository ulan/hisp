module Expr where
import Data.List
import qualified Data.Map as DM 
import Text.Printf

data Expr = A String Pos
          | L [Expr] Pos
  deriving (Eq, Show)

data Value = Boolean Bool
           | Symbol String
           | Number Integer
           | List [Value]
           | Closure [String] Expr Env
  deriving (Eq)

instance Show Value where
  show (Boolean True) = "true" 
  show (Boolean False) = "false" 
  show (Number x) = show x
  show (Symbol x) = x
  show (List xs) = printf "(%s)" $ intercalate " " (map show xs)
  show (Closure params body env) =
    printf "(lambda %s %s)" (intercalate " " params) (show body)

type Env = DM.Map String Value

data Pos = Pos Int Int deriving Eq

instance Show Pos where
  show (Pos line col) = printf "line %d column %d" line col

pos (A _ x) = x
pos (L _ x) = x
