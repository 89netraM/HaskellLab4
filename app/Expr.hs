module Expr where

import Prelude as P

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

----------------------------------
-- C
eval :: Expr -> Double -> Double
eval (Num d) _ = d
eval Var v    = v
eval (Op (Operator op _) e1 e2) v = op (eval e1 v) (eval e2 v)
eval (Fun (Function f _) e) v = f (eval e v)
