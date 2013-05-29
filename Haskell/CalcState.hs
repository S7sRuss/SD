module CalcState where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import StateMaybeMonad

{- Definition of the Abstract Syntax of the language CALCI -}

data STATE = 
    Num Int
  | Add STATE STATE
  | Sub STATE STATE
  | Mul STATE STATE
  | Div STATE STATE

  | Id String
  | Decl String STATE STATE

  | Var STATE
  | Deref STATE
  | Assign STATE STATE
  | Free STATE

  | If STATE STATE STATE
  | While STATE STATE
  | Seq STATE STATE

  | Equal STATE STATE
  | Not STATE
  deriving (Eq,Show)

type Env = [(String,Value)]

type Mem = [(Int,Value)]

type State = Mem

type Loc = Int

data Value = 
    Number Int
  | Ref Loc
  | Boolean Bool
  deriving (Eq,Show)

{- Environment manipulation functions -}

find :: String -> Env -> Maybe Value 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env -> String -> Value -> Env

assoc env x l = ((x,l):env)

{- State manipulation functions -}

newloc :: Value -> StateMaybe State Value
newloc v = SM( \m -> 
               let loc = length m in ((loc,v):m, Just (Ref loc)))

getloc :: Loc -> StateMaybe State Value
getloc l = SM( \m -> (m, Just ((head [v | (l',v)<-m, l==l']))))

setloc :: Loc -> Value -> StateMaybe State Value
setloc loc v = SM (\m ->
                let m' = [(loc,v)] ++ [(l',v) | (l',v) <- m, l' /= loc ] in
                (m',Just v))

toNum :: StateMaybe State Value -> StateMaybe State Int
toNum (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Number n)) -> (s1,Just n)
                          (s1, Just (Ref n)) -> (s1,Nothing)
                          (s1, Just (Boolean b)) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

toRef :: StateMaybe State Value -> StateMaybe State Int
toRef (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Ref n)) -> (s1,Just n)
                          (s1, Just (Number n)) -> (s1,Nothing)
                          (s1, Just (Boolean b)) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

toBool :: StateMaybe State Value -> StateMaybe State Bool
toBool (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Ref n)) -> (s1,Nothing)
                          (s1, Just (Number n)) -> (s1,Nothing)
                          (s1, Just (Boolean b)) -> (s1,Just b)
                          (s1, Nothing) -> (s1,Nothing))


{- Operational Semantics -}


eval :: STATE -> Env -> StateMaybe State Value

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

eval (Equal e e') env = 
     do 
     l <- toNum (eval e env)
     r <- toNum (eval e' env)
     return (Boolean (l == r))

eval (Not e) env = 
     do 
     l <- toBool (eval e env)
     return (Boolean (not l))      


eval (Var e) env = 
     do
     v <- eval e env
     newloc (v)

eval (Deref e) env =
     do
     loc <- toRef (eval e env)
     getloc(loc)

eval (Assign e e') env =
     do
     loc <- toRef (eval e env)
     v <- eval e' env
     setloc loc v

eval (If e e' e'') env = 
     do
     v <- eval e env
     case v of
          Boolean True -> eval e' env
          Boolean False -> eval e'' env

eval (Seq e e') env = 
     do
     eval e env
     eval e' env

eval (While e e') env = 
     do
     v <- eval e env
     case v of 
          Boolean True -> eval (Seq e' (While e e')) env
          Boolean False -> return (Boolean False)
     
runeval :: STATE -> Env -> Maybe Value
runeval e env = let SM(m) = (eval e env) in snd (m [])


a = (Decl "x" (Var (Num 0)) (Seq (While (Not (Equal (Deref (Id "x")) (Num 10))) (Assign (Id "x") (Add (Deref (Id "x")) (Num 1)))) (Deref (Id "x")))) 

 