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

{- 

(fun x -> x+1) (y+2)

-}

type Env a = [(String,a)]

data Value = 
     Number Int
  |  Closure String CalcF (Env Value)
  deriving (Eq,Show)

toNum :: StateMaybe Int Value -> StateMaybe Int Int
toNum (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Number n)) -> (s1,Just n)
                          (s1, Just _) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

{- Environment managment -}


find :: Eq a => String -> Env a -> Maybe a 
find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a
assoc env x l = ((x,l):env)

{- -}


eval :: CalcF -> Env Value -> StateMaybe Int Value

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

eval (Fun x e) env = return (Closure x e env)

eval (Call e e') env = 
      do
      f <- eval e env
      a <- eval e' env
      case f of 
        Closure x e'' env' -> eval e'' (assoc env' x a)
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
