module CalcState where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import StateMaybeMonad
import MaybeMessageMonad

{- Definition of the Abstract Syntax of the language CALCI -}

data CalcState = 
    Num Int
  | Add CalcState CalcState
  | Sub CalcState CalcState
  | Mul CalcState CalcState
  | Div CalcState CalcState

  | Id String
  | Decl String CalcState CalcState

  | Var CalcState
  | Deref CalcState
  | Assign CalcState CalcState
  | Free CalcState

  | If CalcState CalcState CalcState
  | While CalcState CalcState
  | Seq CalcState CalcState

  | Equal CalcState CalcState
  | Not CalcState
  deriving (Eq,Show)

data Type = 
     IntType 
  |  BoolType
  |  RefType Type 
  deriving (Eq, Show)

type Env a = [(String,a)]

type Mem = [(Int,Value)]

{- Note that the implementation of the memory as a list is for demonstration purposes only -}

type Loc = Int

data Value = 
    Number Int
  | Ref Loc
  | Boolean Bool
  deriving (Eq,Show)

{- Environment manipulation functions -}

find :: Eq a => String -> Env a -> Maybe a 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a

assoc env x l = ((x,l):env)

{- State manipulation functions -}

newloc :: Value -> StateMaybe Mem Value
newloc v = SM( \m -> 
               let loc = length m in ((loc,v):m, Just (Ref loc)))

getloc :: Loc -> StateMaybe Mem Value
getloc l = SM( \m -> (m, Just ((head [v | (l',v)<-m, l==l']))))

setloc :: Loc -> Value -> StateMaybe Mem Value
setloc loc v = SM (\m ->
                let m' = [(loc,v)] ++ [(l',v) | (l',v) <- m, l' /= loc ] in
                (m',Just v))

toNum :: StateMaybe Mem Value -> StateMaybe Mem Int
toNum (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Number n)) -> (s1,Just n)
                          (s1, Just (Ref n)) -> (s1,Nothing)
                          (s1, Just (Boolean b)) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

toRef :: StateMaybe Mem Value -> StateMaybe Mem Int
toRef (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Ref n)) -> (s1,Just n)
                          (s1, Just (Number n)) -> (s1,Nothing)
                          (s1, Just (Boolean b)) -> (s1,Nothing)
                          (s1, Nothing) -> (s1,Nothing))

toBool :: StateMaybe Mem Value -> StateMaybe Mem Bool
toBool (SM(f)) = SM( \s0 -> case f s0 of
                          (s1, Just (Ref n)) -> (s1,Nothing)
                          (s1, Just (Number n)) -> (s1,Nothing)
                          (s1, Just (Boolean b)) -> (s1,Just b)
                          (s1, Nothing) -> (s1,Nothing))


{- Operational Semantics -}


eval :: CalcState -> Env Value-> StateMaybe Mem Value

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
     
runeval :: CalcState -> Env Value -> Maybe Value
runeval e env = let SM(m) = (eval e env) in snd (m [])

{- 

decl x = var(0) in
     While (not (x = 10)) !x := !x + 1
     ;
     !!x
-}

a_error =  
        (Decl "x" 
              (Var (Num 0)) 
              (Seq 
                   (While 
                          (Not (Equal ((Id "x")) (Num 10))) 
                          (Assign (Deref (Id "x")) (Add (Deref (Id "x")) (Num 1)))) 
                   (Deref (Deref (Id "x")))))





{-

decl x = var(0) in
     While (not !x = 10) x := !x + 1
     ;
     !x
-}

a = (Decl "x" (Var (Num 0)) (Seq (While (Not (Equal (Deref (Id "x")) (Num 10))) (Assign (Id "x") (Add (Deref (Id "x")) (Num 1)))) (Deref (Id "x")))) 

{- runeval a [] -}




toInt:: MaybeMessage Type -> MaybeMessage Type

toInt (Message s) = Message s
toInt (Result IntType) = (Result IntType)
toInt (Result _) = Message "Not an Int value."


toBoolT:: MaybeMessage Type -> MaybeMessage Type

toBoolT (Message s) = Message s
toBoolT (Result BoolType) = (Result BoolType)
toBoolT (Result _) = Message "Not a Bool value."


typecheck :: CalcState -> Env Type -> MaybeMessage Type

typecheck (Id x) env = 
          case find x env of
               Just v -> Result v
               Nothing -> Message ("Identifier "++x++ " not found.")

typecheck (Decl x e e') env =
          do
          t <- typecheck e env
          typecheck e' (assoc env x t)

typecheck (Num n) env = return IntType

typecheck (Add e e') env = 
          do 
          t <- toInt (typecheck e env)
          t' <- toInt (typecheck e' env)
          return IntType

typecheck (Sub e e') env = 
          do 
          t <- toInt (typecheck e env)
          t' <- toInt (typecheck e' env)
          return IntType

typecheck (Mul e e') env = 
          do 
          t <- toInt (typecheck e env)
          t' <- toInt (typecheck e' env)
          return IntType

typecheck (Div e e') env = 
          do 
          t <- toInt (typecheck e env)
          t' <- toInt (typecheck e' env)
          return IntType

typecheck (Equal e e') env = 
          do 
          t <- typecheck e env
          t' <- typecheck e' env
          if t == t' then return BoolType
          else Message "Comparison between values of non-equal types."


typecheck (Not e) env = 
          do
          t <- toBoolT (typecheck e env)
          return BoolType
          
typecheck (Var e) env = 
          do 
          t <- typecheck e env
          return (RefType t);

typecheck (Deref e) env = 
          do 
          t <- typecheck e env
          case t of
             RefType t' -> return t'
             _ -> Message "Dereferencing a non-reference value."

typecheck (Assign e e') env = 
          do
          t <- typecheck e env
          t' <- typecheck e' env
          case t of
             RefType t'' | t'' == t' -> return t' 
             RefType _ -> Message "Type mismatch on assignment."
             _ -> Message "Assigning a non-reference value."

typecheck (If e e' e'') env = 
          do 
          t_cond <- typecheck e env
          t_then <- typecheck e' env
          t_else <- typecheck e'' env
          case t_cond of
               BoolType | t_then == t_else -> return t_then
               BoolType -> Message "Type mismatch on conditional branches."
               _ -> Message "Not a Bool value in if condition." 
          
typecheck (Seq e e') env = 
          do
          t <- typecheck e env
          t'<- typecheck e' env
          return t'

typecheck (While e e') env = 
          do 
          t_cond <- typecheck e env
          t_body <- typecheck e' env
          case t_cond of
               BoolType -> return BoolType
               _ -> Message "Not a Bool value in while condition."
          
