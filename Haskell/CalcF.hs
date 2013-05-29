module CalcF where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import StateMaybeMonad

{- Definition of the Abstract Syntax of the language CalcF -}

data CalcF = 
    Num Int
  | Add CalcF CalcF
  | Sub CalcF CalcF
  | Mul CalcF CalcF
  | Div CalcF CalcF

  | Id String
  | Decl String CalcF CalcF

  | Fun String CalcF
  | Call CalcF CalcF
  deriving (Eq,Show)

{- Examples -}

a = Id "x"

b = Decl "x" (Num 1) (Add a (Num 1))

c = Decl "x" (Id "x") b

d = Fun "y" (Add (Id "y") (Num 1)) {- fun y -> y+1 -}

e = Call (Fun "x" (Call (Id "x") (Num 2))) d

{- Free identifiers -}

free :: CalcF -> [String]
free (Id x)        = [x]
free (Decl x e e') = (free e) ++ (filter (\y -> x /= y) (free e'))
free (Num n)       = []
free (Add e e')    = (free e) ++ (free e')
free (Sub e e')    = (free e) ++ (free e')
free (Mul e e')    = (free e) ++ (free e')
free (Div e e')    = (free e) ++ (free e')
free (Fun x e )    = (filter (\y -> x /= y) (free e))
free (Call e e')   = (free e) ++ (free e')


{- Substitution function, does not avoid identifier capturing -}

subst :: CalcF -> String -> CalcF -> CalcF

subst e x (Id y) | x == y            = e
subst e x (Id y) | x /= y            = Id y

subst e x (Decl y e' e'') | x == y   = (Decl y (subst e x e') e'')
subst e x (Decl y e' e'') | x /= y   = (Decl y (subst e x e') (subst e x e''))

subst e x (Num n)                    = Num n
subst e x (Add e' e'')               = Add (subst e x e') (subst e x e'')
subst e x (Sub e' e'')               = Sub (subst e x e') (subst e x e'')
subst e x (Mul e' e'')               = Mul (subst e x e') (subst e x e'')
subst e x (Div e' e'')               = Div (subst e x e') (subst e x e'')

subst e x (Fun y e') | x == y        = (Fun y e')
subst e x (Fun y e') | x /= y        = (Fun y (subst e x e'))

subst e x (Call e' e'')              = Call (subst e x e') (subst e x e'')


toNum :: StateMaybe Int CalcF -> StateMaybe Int Int
toNum (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Num n)) -> (s1,Just n)
                          (s1, Just _) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

 -------------------------------------------------------------------------------
{- Semantics for closed expressions and capture avoiding substitutions         -}
 -------------------------------------------------------------------------------

eval :: CalcF -> StateMaybe Int CalcF

eval (Num n) = return (Num n)

eval (Add e e') = 
     do 
     l <- toNum (eval e)
     r <- toNum (eval e')
     return (Num (l+r))

eval (Sub e e') = 
     do 
     l <- toNum (eval e)
     r <- toNum (eval e')
     return (Num (l-r))

eval (Mul e e') =
     do 
     l <- toNum (eval e)
     r <- toNum (eval e')
     return (Num (l*r))

eval (Div e e') = 
     do 
     l <- toNum (eval e)
     r <- toNum (eval e')
     if r == 0 then raise_error 
     else return (Num (div l r))

eval (Decl x e e') = 
     do
     x' <- newId
     e'' <- return (subst (Id x') x e') 
-- replacing the declared identifier by a new one to avoid capturing of identifiers
     eval (subst e x' e'')

eval (Id x) = raise_error

eval (Fun x e ) = return (Fun x e)

eval (Call e e' ) = 
      do
      f <- eval e 
      a <- eval e'
      case f of 
        Fun x e'' -> eval (subst a x e'')
        _ -> raise_error

runstate :: CalcF -> Maybe CalcF
runstate e = let SM(f) = (eval e) in snd (f 0)

{-

decl x=1 in			 
 decl f = (fun y -> y+x) in
   decl g = (fun x -> x+f(x)) 
     in g(2)

-}

g2 = Call (Id "g") (Num 2)

dg = Decl "g" (Fun "x" (Add (Id "x") (Call (Id "f") (Id "x")))) g2

df = Decl "f" (Fun "y" (Add (Id "y") (Id "x"))) dg

dx = Decl "x" (Num 1) df

{- runstate dx -}

xx = Add (Id "x") (Id "x")

dxx = Decl "x" (Call (Id "g") (Num 2)) xx 

dgx = Decl "g" (Fun "y" (Add (Call (Id "f") (Id "y")) (Num 2))) dxx

dfx = Decl "f" (Fun "x" (Add (Id "x") (Num 1))) dgx

