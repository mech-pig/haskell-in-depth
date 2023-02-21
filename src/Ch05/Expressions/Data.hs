module Ch05.Expressions.Data where

data Expr a = Lit a | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a)

exec :: Expr Integer -> Integer
exec (Lit i) = i
exec (Add e1 e2) = exec e1 + exec e2
exec (Mul e1 e2) = exec e1 * exec e2