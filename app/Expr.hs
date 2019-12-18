module Expr where

import Prelude as P
import qualified Test.QuickCheck as Q
import Parsing
import Data.Maybe (fromJust)

------------------------------
-- A
data Operator = Operator {
    opFun :: Double -> Double -> Double
  , opSym :: String
}
data Function = Function {
    funFun :: Double -> Double
  , funSym :: String
}

data Expr = Num Double
          | Var
          | Op Operator Expr Expr
          | Fun Function Expr
  deriving Eq


x :: Expr
x = Var

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Op (Operator (+) "+")

mul :: Expr -> Expr -> Expr
mul = Op (Operator (*) "*")

sin :: Expr -> Expr
sin = Fun (Function P.sin "sin")

cos :: Expr -> Expr
cos = Fun (Function P.cos "cos")

----------------------------------
-- B
showExpr :: Expr -> String
showExpr (Num d) = show d
showExpr Var = "x"
showExpr (Op (Operator _ "*") (Op (Operator op "+") e1 e2) e) =
  "(" ++ showExpr (Op (Operator op "+") e1 e2) ++ ")" ++ "*" ++ showExpr e
showExpr (Op (Operator _ "*") e (Op (Operator op "+") e1 e2)) =
    showExpr e ++ "*" ++ "(" ++ showExpr (Op (Operator op "+") e1 e2) ++ ")"
showExpr (Op op e1 e2) = showExpr e1 ++ opSym op ++ showExpr e2
showExpr (Fun fun (Op op e1 e2)) =
  funSym fun ++ "(" ++ showExpr (Op op e1 e2) ++ ")"
showExpr (Fun fun e) = funSym fun ++ showExpr e

instance Show Expr where
  show = showExpr

----------------------------------
-- C
eval :: Expr -> Double -> Double
eval (Num d) _ = d
eval Var v    = v
eval (Op (Operator op _) e1 e2) v = op (eval e1 v) (eval e2 v)
eval (Fun (Function f _) e) v = f (eval e v)

----------------------------------
-- D
readExpr :: String -> Maybe Expr
readExpr s = parse expr s >>= \(expr, _) -> return expr

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = p >>= \r -> return [r] <|> return []

number :: Parser Double
number = do
  before <- oneOrMore digit
  (do
    char '.'
    after <- oneOrMore digit
    (do
      char 'e'
      neg <- zeroOrOne (char '-')
      exp <- oneOrMore digit
      return $ read (before ++ "." ++ after ++ "e" ++ neg ++ exp)
      ) <|> (return $ read (before ++ "." ++ after))
    ) <|> return (read before)

-- | Parses the specified string or fails.
string :: String -> Parser String
string (c:cs) = char c >>= \s -> string cs >>= \ss -> return $ s:ss
string _      = return ""

{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "x" | "(" expr ")" | "sin(" expr ")" | "cos(" expr ")".
-}

expr :: Parser Expr
expr = foldl1 add <$> chain term (char '+')

term :: Parser Expr
term = foldl1 mul <$> chain factor (char '*')

factor :: Parser Expr
factor = (num <$> number) <|> char '(' *> expr <* char ')'
           <|> (char 'x' >> (return x))
           <|> string "sin" *> char '(' *> (Expr.sin <$> expr) <* char ')'
           <|> string "cos" *> char '(' *> (Expr.cos <$> expr) <* char ')'

----------------------------------
-- E
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = (fromJust $ readExpr $ showExpr expr) == expr

arbExpr :: Int -> Q.Gen Expr
arbExpr s = Q.frequency [(1, rOne), (s, rExp)]
  where
    rOne = do
      o <- Q.elements [return x, rNum]
      o
    rNum = do
      n <- Q.choose (0.0, 99.0)
      return $ num n
    rExp = do
      ex <- Q.elements [rOp, rFun]
      ex
        where
          s' = s `div` 2
          rOp = do
            op <- Q.elements [add, mul]
            e1 <- arbExpr s'
            e2 <- arbExpr s'
            return $ op e1 e2
          rFun = do
            fun <- Q.elements [Expr.sin, Expr.cos]
            e <- arbExpr s'
            return $ fun e

instance Q.Arbitrary Expr where
  arbitrary = Q.sized arbExpr

instance Eq Operator where
  (Operator _ a) == (Operator _ b) = a == b

instance Eq Function where
  (Function _ a) == (Function _ b) = a == b

----------------------------------
-- F
simplify :: Expr -> Expr
simplify (Num n) = num n
simplify Var     = x
simplify (Op (Operator op s) e1 e2)
  | isNum s1 && isNum s2 = num (op (getNum s1) (getNum s2))
  | otherwise            = Op (Operator op s) s1 s2
  where
    s1 = simplify e1
    s2 = simplify e2
simplify (Fun (Function f s) e)
  | isNum simp = num (f (getNum simp))
  | otherwise  = Fun (Function f s) simp
  where
    simp = simplify e


isNum :: Expr -> Bool
isNum (Num _) = True
isNum _       = False

getNum :: Expr -> Double
getNum (Num n) = n

prop_simplify :: Expr -> Double -> Bool
prop_simplify e x = eval (simplify e) x == eval e x

----------------------------------
-- G
differentiate :: Expr -> Expr
differentiate (Op (Operator _ "+") e1 e2) = simplify $ add (differentiate e1) (differentiate e2)
differentiate (Op (Operator _ "*") e1 e2) = simplify $ add (mul (differentiate e1) e2) (mul e1 (differentiate e2))
differentiate (Fun (Function _ "sin") e)  = simplify $ Expr.cos (differentiate e)
differentiate (Fun (Function _ "cos") e)  = simplify $ mul (num (-1)) (Expr.sin (differentiate e))
differentiate Var                         = num 1
differentiate _                           = num 0
