module Expr where

import Prelude as P
import qualified Test.QuickCheck as Q
import Parsing

------------------------------
-- A
--type Function = Double -> Double
--type Operator = Double -> Double -> Double
data Operator = Operator (Double -> Double -> Double) String
data Function = Function (Double -> Double) String

data Expr = Num Double
          | Var
          | Op Operator Expr Expr
          | Fun Function Expr


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
showExpr (Op (Operator _ r) e1 e2) =
  showExpr e1 ++ " " ++ r ++ " " ++ showExpr e2
showExpr (Fun (Function _ r) e) = r ++ "(" ++ showExpr e ++ ")"

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

number :: Parser Double
number = read <$> oneOrMore (digit <|> char '.' <|> char 'e' <|> char '-')

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
-- Waiting for Part D
-- prop_ShowReadExpr :: Expr -> Bool
-- prop_ShowReadExpr expr = (readExpr $ showExpr expr) == expr

arbExpr :: Int -> Q.Gen Expr
arbExpr s = Q.frequency [(1, rNum), (s, rExp)]
  where
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