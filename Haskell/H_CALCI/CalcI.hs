module CalcI where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import StateMaybeMonad

{- Definition of the Abstract Syntax of the language CALCI -}

data CALCI = 
    Num Int
  | Add CALCI CALCI
  | Sub CALCI CALCI
  | Mul CALCI CALCI
  | Div CALCI CALCI

  | Id String
  | Decl String CALCI CALCI
  deriving Show

{- Examples -}

a = Id "x"

b = Decl "x" (Num 1) (Add a (Num 1))

c = Decl "x" (Id "x") b

-- decl x = 1 in x
-- decl x = x in decl x = 1 in x

{- Free identifiers -}

free :: CALCI -> [String]
free (Id x)        = [x]
free (Decl x e e') = (free e) ++ (filter (\y -> x /= y) (free e'))
free (Num n)       = []
free (Add e e')    = (free e) ++ (free e')
free (Sub e e')    = (free e) ++ (free e')
free (Mul e e')    = (free e) ++ (free e')
free (Div e e')    = (free e) ++ (free e')

{- Substitution function, does not avoid identifier capturing -}

subst :: CALCI -> String -> CALCI -> CALCI

subst e x (Id y) | x == y            = e
subst e x (Id y) | x /= y            = Id y

subst e x (Decl y e' e'') | x == y   = (Decl y (subst e x e') e'')
subst e x (Decl y e' e'') | x /= y   = (Decl y (subst e x e') (subst e x e''))

subst e x (Num n)                    = Num n
subst e x (Add e' e'')               = Add (subst e x e') (subst e x e'')
subst e x (Sub e' e'')               = Sub (subst e x e') (subst e x e'')
subst e x (Mul e' e'')               = Mul (subst e x e') (subst e x e'')
subst e x (Div e' e'')               = Div (subst e x e') (subst e x e'')

 -------------------------------------------------------------------------------
{- Semantics for closed expressions                                            -}
 -------------------------------------------------------------------------------

eval :: CALCI -> StateMaybe Int Int

eval (Num n) = return n

eval (Add e e') = 
     do 
     l <- (eval e)
     r <- (eval e')
     return (l+r)

eval (Sub e e') = 
     do 
     l <- (eval e)
     r <- (eval e')
     return (l-r)

eval (Mul e e') =
     do 
     l <- (eval e)
     r <- (eval e')
     return (l*r)

eval (Div e e') = 
     do 
     l <- (eval e)
     r <- (eval e')
     if r == 0 then raise_error 
     else return (div l r)

eval (Decl x e e') = eval e''
     where e'' = subst e x e'
-- This version does not avoid capturing 

eval (Id x) = raise_error


-- subst (Num 1) "x" (Id "x") == (Num 1)
-- subst (Num 1) "y" (Id "x") == (Id "x")

 -------------------------------------------------------------------------------
{- Semantics for closed expressions and capture avoiding substitutions         -}
 -------------------------------------------------------------------------------

eval' :: CALCI -> StateMaybe Int Int

eval' (Num n) = return n

eval' (Add e e') = 
     do 
     l <- (eval' e)
     r <- (eval' e')
     return (l+r)

eval' (Sub e e') = 
     do 
     l <- (eval' e)
     r <- (eval' e')
     return (l-r)

eval' (Mul e e') =
     do 
     l <- (eval' e)
     r <- (eval' e')
     return (l*r)

eval' (Div e e') = 
     do 
     l <- (eval' e)
     r <- (eval' e')
     if r == 0 then raise_error 
     else return (div l r)

eval' (Decl x e e') = 
     do
     x' <- newId
     e'' <- return (subst (Id x') x e') 
-- replacing the declared identifier by a new one to avoid capturing of identifiers
     eval' (subst e x' e'')

eval' (Id x) = raise_error

runstate :: CALCI -> Maybe Int
runstate e = let SM(f) = (eval' e) in snd (f 0)


 -------------------------------------------------------------------------------
{- Semantics for open expressions with environments                            -}
 -------------------------------------------------------------------------------

{- Environment managment -}

find :: String -> [(String,Int)] -> Maybe Int 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

{- -}

evalEnv :: CALCI -> [(String,Int)] -> StateMaybe Int Int

evalEnv (Id x) env = 
        case find x env of 
          Nothing -> raise_error 
          Just v -> return v

evalEnv (Decl x e e') env = 
        do 
        l <- evalEnv e env 
        evalEnv e' ((x,l):env)

evalEnv (Num n) env = return n

evalEnv (Add e e') env = 
     do 
     l <- (evalEnv e env)
     r <- (evalEnv e' env)
     return (l+r)

evalEnv (Sub e e') env = 
     do 
     l <- (evalEnv e env)
     r <- (evalEnv e' env)
     return (l-r)

evalEnv (Mul e e') env =
     do 
     l <- (evalEnv e env)
     r <- (evalEnv e' env)
     return (l*r)

evalEnv (Div e e') env = 
     do 
     l <- (evalEnv e env)
     r <- (evalEnv e' env)
     if r == 0 then raise_error 
     else return (div l r)

runstateEnv :: CALCI -> [(String,Int)] -> Maybe Int
runstateEnv e env = let SM(f) = (evalEnv e env) in snd (f 0)

-- runstateEnv (Decl "x" (Num 1) (Add (Id "x") (Num 1))) []

-- runstateEnv (Decl "x" (Num 3) (Add b (Id "x"))) []
