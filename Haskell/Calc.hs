module Calc where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

{- Definition of the Abstract Syntax of the language CALC -}

data CALC = 
    Num Int
  | Add CALC CALC
  | Sub CALC CALC
  | Mul CALC CALC
  | Div CALC CALC
  deriving Show

{- Examples -}

a = (Num 1)

b = (Add (Num 1) (Sub (Num 3) (Num 2)))

 -------------------------------------------------------------------------------
{- Semantics                                                                   -}
 -------------------------------------------------------------------------------

eval :: CALC -> Int
eval (Num n) = n
eval (Add e e') = (eval e) + (eval e')
eval (Sub e e') = (eval e) - (eval e')
eval (Mul e e') = (eval e) * (eval e')
eval (Div e e') = div (eval e) (eval e')
-- division by zero causes an exception to occur

 -------------------------------------------------------------------------------
{- Abstract syntax of results                                                  -}
 -------------------------------------------------------------------------------

data Result = 
       Value Int
     | Error
     deriving Show

 -------------------------------------------------------------------------------
{- Semantics with Result (Int or Error)                                        -}
 -------------------------------------------------------------------------------

evalCase :: CALC -> Result

evalCase (Num n) = Value n

evalCase (Add e e') = 
         case (evalCase e, evalCase e') of
              (Value n, Value n') -> Value (n+n')
              (_ , _) -> Error

evalCase (Sub e e') = 
         case (evalCase e, evalCase e') of
              (Value n, Value n') -> Value (n-n')
              (_ , _) -> Error

evalCase (Mul e e') = 
         case (evalCase e, evalCase e') of
              (Value n, Value n') -> Value (n*n')
              (_ , _) -> Error

evalCase (Div e e') = 
         case (evalCase e, evalCase e') of
              (Value n, Value 0) -> Error
              (Value n, Value n') -> Value (div n n')
              (_ , _) -> Error


 -------------------------------------------------------------------------------
{- Semantics with Result (Int or Error) using Maybe Monad                      -}
 -------------------------------------------------------------------------------

evalErr :: CALC -> Maybe Int

evalErr (Num n) = Just n

evalErr (Add e e') = do 
        l <- evalErr e 
        r <- evalErr e'
        return (l+r)

evalErr (Sub e e') = do 
        l <- evalErr e 
        r <- evalErr e'
        return (l-r)

evalErr (Mul e e') = do 
        l <- evalErr e 
        r <- evalErr e'
        return (l*r)

evalErr (Div e e') = do 
        l <- evalErr e 
        r <- evalErr e'
        if r == 0 then Nothing else Just (div l r)

-- evalErr (Num 1)
-- evalErr (Add (Num 1) (Sub (Num 3) (Num 2)))

 -------------------------------------------------------------------------------
{- Definition of a specific Error Monad                                        -}
 -------------------------------------------------------------------------------

data ErrorMonad a = 
       Result a
     | Wrong
     deriving Show

instance Monad ErrorMonad
  where
    -- (>>=) :: ErrorMonad a -> (a -> ErrorMonad b) -> ErrorMonad b
    p >>= k  =  case p of 
                  Result a -> k a 
                  Wrong -> Wrong
     			   	
    -- return :: a -> ErrorMonad a
    return a = Result a

raise_error :: ErrorMonad a
raise_error = Wrong

 -------------------------------------------------------------------------------
{- Semantics using the ErrorMonad                                              -}
 -------------------------------------------------------------------------------

evalWrong :: CALC -> ErrorMonad Int

evalWrong (Num n) = return n

evalWrong (Add e e') = do 
        l <- evalWrong e 
        r <- evalWrong e'
        return (l+r)

evalWrong (Sub e e') = do 
        l <- evalWrong e 
        r <- evalWrong e'
        return (l-r)

evalWrong (Mul e e') = do 
        l <- evalWrong e 
        r <- evalWrong e'
        return (l*r)

evalWrong (Div e e') = do 
        l <- evalWrong e 
        r <- evalWrong e'
        if r == 0 then raise_error else return (div l r)