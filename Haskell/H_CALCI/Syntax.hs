module Syntax where

data Exp 
  = Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Num Int
  deriving (Eq,Show)

surround :: String -> String
surround s = "( "++ s ++ " )"

unparse :: Exp -> String
unparse (Num n) = show n
unparse (Add e e') = surround ((unparse e) ++ " + " ++ (unparse e'))
unparse (Sub e e') = surround ((unparse e) ++ " - " ++ (unparse e'))
unparse (Mul e e') = surround ((unparse e) ++ " * " ++ (unparse e'))
unparse (Div e e') = surround ((unparse e) ++ " / " ++ (unparse e'))

