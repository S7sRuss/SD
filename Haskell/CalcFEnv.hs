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

data Value = 
     Number Int
  |  Abs String CalcF
  deriving (Eq,Show)

{- Examples -}

a = Id "x"

b = Decl "x" (Num 1) (Add a (Num 1))

c = Decl "x" (Id "x") b

d = Fun "x" (Add (Id "x") (Num 1))

toNum :: StateMaybe Int Value -> StateMaybe Int Int
toNum (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Number n)) -> (s1,Just n)
                          (s1, Just (Abs _ _)) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

 -------------------------------------------------------------------------------
{- Semantics for open expressions with environments                            -}
 -------------------------------------------------------------------------------

{- Environment managment -}

type Env = [(String,Value)]

find :: String -> Env -> Maybe Value 
find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env -> String -> Value -> Env
assoc env x l = ((x,l):env)

{- -}

eval :: CalcF -> Env -> StateMaybe Int Value

eval (Id x) env = 
        case find x env of 
          Nothing -> raise_error 
          Just v -> return v

eval (Decl x e e') env = 
        do 
        l <- eval e env 
        eval e' (assoc env x l)

eval (Num n) env = return (Number n)

eval (Add e e') env = 
     do 
     l <- toNum (eval e env)
     r <- toNum (eval e' env)
     return (Number (l+r))

eval (Sub e e') env = 
     do 
     l <- toNum (eval e env)
     r <- toNum (eval e' env)
     return (Number (l-r))

eval (Mul e e') env =
     do 
     l <- toNum (eval e env)
     r <- toNum (eval e' env)
     return (Number (l*r))

eval (Div e e') env = 
     do 
     l <- toNum (eval e env)
     r <- toNum (eval e' env)
     if r == 0 then raise_error 
     else return (Number (div l r))

eval (Fun x e) env = return (Abs x e)

eval (Call e e') env = 
      do
      f <- eval e env
      a <- eval e' env
      case f of 
        Abs x e'' -> eval e'' (assoc env x a)
        _ -> raise_error     

runstate :: CalcF -> Maybe Value
runstate e = let SM(f) = (eval e []) in snd (f 0)


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

