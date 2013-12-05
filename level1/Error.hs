module Error where
import Control.Monad.Error
import Expr
import Text.Printf

data EvalError = UnboundIdentifier String Pos
               | ExpectBoolean Value Pos
               | ExpectNumber Value Pos
               | ExpectSymbol Value Pos
               | ExpectList Value Pos
               | ExpectListOf Int Value Pos
               | ExpectListOfAtLeast Int Value Pos
               | ExpectArguments Int Int Pos
               | ExpectArgumentsAtLeast Int Int Pos
               | ExpectClosure Value Pos
               | DivisionByZero Pos
               | OtherError String

instance Error EvalError where
  noMsg    = OtherError "no message"
  strMsg s = OtherError s

instance Show EvalError where
  show (UnboundIdentifier x p) =
    printf "Unbound identifier %s at %s" x (show p)
  show (ExpectBoolean x p) =
    printf "Expect boolean at %s, found %s" (show p) (show x)
  show (ExpectNumber x p) =
    printf "Expect number at %s, found %s" (show p) (show x)
  show (ExpectSymbol x p) =
    printf "Expect string at %s, found %s" (show p) (show x)
  show (ExpectList x p) =
    printf "Expect list at %s, found %s" (show p) (show x)
  show (ExpectListOf n x p) =
    printf "Expect list of length %d at %s, found %s" n (show p) (show x)
  show (ExpectListOfAtLeast n x p) =
    printf "Expect list of length >= %d at %s, found %s" n (show p) (show x)
  show (ExpectArguments n args p) =
    printf "Expect %d arguments for %s at %s, got %d" n (show p) args
  show (ExpectArgumentsAtLeast n args p) =
    printf "Expect at least %d arguments for at %s, got %d" n (show p) args
  show (ExpectClosure x p) =
    printf "Expect function at %s, found %s" (show p) (show x)
  show (DivisionByZero p) =
    printf "Division by zero at %s" (show p)
  show (OtherError x) = printf "Error %s" x
