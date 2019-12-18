module Expr where

import Prelude as P
import qualified Test.QuickCheck as Q
import Parsing
import Data.Maybe (fromJust)
import Data.Char as C (isSpace)

------------------------------
-- A
data Operator = Operator {
    opFun :: Double -> Double -> Double
  , opSym :: String
}

showOp :: Operator -> String
showOp = opSym

instance Show Operator where
  show = showOp

instance Eq Operator where
  (Operator _ a) == (Operator _ b) = a == b

data Function = Function {
    funFun :: Double -> Double
  , funSym :: String
}

showFun :: Function -> String
showFun = funSym

instance Show Function where
  show = showFun

instance Eq Function where
  (Function _ a) == (Function _ b) = a == b

data Expr = Num Double
          | Var
          | Op Operator Expr Expr
          | Fun Function Expr

instance Eq Expr where
  (==) = eqExpr

instance Show Expr where
  show = showExpr

eqExpr :: Expr -> Expr -> Bool
eqExpr x y = eqStructure (assoc x) (assoc y)

assoc :: Expr -> Expr
assoc (Op op (Op op' e1 e2) e3) | op == op'
    = assoc (Op op e1 (Op op' e2 e3))
assoc (Op op e1 e2) = Op op (assoc e1) (assoc e2)
assoc (Fun f e) = Fun f (assoc e)
assoc Var = Var
assoc (Num d) = Num d

eqStructure :: Expr -> Expr -> Bool
eqStructure (Op op e1 e2) (Op op' e1' e2')
  = op == op' && eqStructure e1 e1' && eqStructure e2 e2'
eqStructure (Fun f e) (Fun f' e') = f == f' && eqStructure e e'
eqStructure (Num a) (Num b) = a == b
eqStructure Var Var = True
eqStructure _ _ = False

------------------------------------------------------------

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
showExpr (Op (Operator _ "*") e (Op (Operator op "+") e1 e2)) =
  "(" ++  showExpr e ++ ")" ++ "*" ++ "(" ++ showExpr (Op (Operator op "+") e1 e2) ++ ")"
showExpr (Op (Operator _ "*") (Op (Operator op "+") e1 e2) e) =
  "(" ++ showExpr (Op (Operator op "+") e1 e2) ++ ")" ++ "*" ++ "(" ++ showExpr e ++ ")"
showExpr (Op op e1 e2) = showExpr e1 ++ opSym op ++ showExpr e2
showExpr (Fun fun (Op op e1 e2)) =
  funSym fun ++ "(" ++ showExpr (Op op e1 e2) ++ ")"
showExpr (Fun fun e) = funSym fun ++ " " ++ showExpr e
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
readExpr s = parse expr (filter (not . C.isSpace) s) >>= \(expr, _) -> return expr

number :: Parser Double
number = readsP

-- | Parses the specified string or fails.
string :: String -> Parser String
string (c:cs) = char c >>= \s -> string cs >>= \ss -> return $ s:ss
string _      = return ""

{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "x" | "(" expr ")" | "sin" expr | "cos" expr.
-}

expr :: Parser Expr
expr = foldl1 add <$> chain term (char '+')

term :: Parser Expr
term = foldl1 mul <$> chain factor (char '*')

factor :: Parser Expr
factor = (num <$> number) <|> char '(' *> expr <* char ')'
           <|> (char 'x' >> (return x))
           <|> string "sin" *> (Expr.sin <$> factor)
           <|> string "cos" *> (Expr.cos <$> factor)

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

----------------------------------
-- F
simplify :: Expr -> Expr
simplify (Num n) = num n
simplify Var     = x
simplify (Op op e1 e2)
  | isNum s1 && isNum s2 = num (opFun op (getNum s1) (getNum s2))
  | opSym op == "+" && s1 == Num 0 = s2
  | opSym op == "+" && s2 == Num 0 = s1
  | opSym op == "*" && s1 == Num 1 = s2
  | opSym op == "*" && s2 == Num 1 = s1
  | opSym op == "*" && s1 == Num 0 = num 0
  | opSym op == "*" && s2 == Num 0 = num 0
  | otherwise            = Op op s1 s2
  where
    s1 = simplify e1
    s2 = simplify e2
simplify (Fun f e)
  | isNum simp = num (funFun f (getNum simp))
  | otherwise  = Fun f simp
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
differentiate (Fun (Function _ "sin") e)  = simplify $ mul (Expr.cos e) (differentiate e)
differentiate (Fun (Function _ "cos") e)  = simplify $ mul (mul (num (-1)) (Expr.sin e)) (differentiate e)
differentiate Var                         = num 1
differentiate _                           = num 0