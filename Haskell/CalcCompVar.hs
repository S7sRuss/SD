module CalcCompVar where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import StateMaybeMonad

{- Definition of the Abstract Syntax of the typed language CalcF -}


data CalcI = 
    Num Int
  | Add CalcI CalcI 
  | Sub CalcI CalcI
  | Mul CalcI CalcI
  | Div CalcI CalcI

  | Id String 
  | Var String CalcI CalcI
  deriving (Eq, Show)

{-

@.str = private constant [4 x i8] c"%d\0A\00", align 1 ; <[4 x i8]*> [#uses=1]

define i32 @main() nounwind {

  %0 = add i32 1, 18                         ; <i32> [#uses=1]

  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0), i32 %0) nounwind ; <i32> [#uses=0]
  ret i32 0
}

declare i32 @printf(i8*, ...) nounwind


For each expression we must provide a list of instructions 
that produce the value and the register that holds it (or a literal).

-}

data Dest = 
    Reg Int {- Registers -}
  | RegV String Int {- Local Var -}
  | I32 Int {- Constants -}
  deriving (Eq, Show)

data Op = 
    Add_i32 Dest Dest Dest
  | Sub_i32 Dest Dest Dest
  | Mul_i32 Dest Dest Dest
  | Div_i32 Dest Dest Dest
  | New_var Dest Dest
  | Get_var Dest Dest
  deriving (Eq, Show)

{----}

type Env a = [(String,a)]

find :: Eq a => String -> Env a -> Maybe a 
find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a
assoc env x l = ((x,l):env)

{----}

dest2Str :: Dest -> String

dest2Str (I32 n) = show n

dest2Str (Reg n) = "%"++(show n)

dest2Str (RegV x n) = "%"++x++"."++(show n)

{----}

op2Str :: Op -> String

op2Str (Add_i32 d d' d'') = "  "++(dest2Str d)++" = add i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (Sub_i32 d d' d'') = "  "++(dest2Str d)++" = sub i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (Mul_i32 d d' d'') = "  "++(dest2Str d)++" = mul i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (Div_i32 d d' d'') = "  "++(dest2Str d)++" = sdiv i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (New_var d d') = 
  "  "++(dest2Str d)++" = alloca i32\n"++
  "  store i32 "++(dest2Str d')++", i32* "++(dest2Str d)++", align 4"
{-
  %x.0 = alloca i32                                 ; <i32*> [#uses=2]
  store i32 %e, i32* %x.0, align 4
-}


op2Str (Get_var d d') = 
  "  "++(dest2Str d)++" = load i32* "++(dest2Str d')++", align 4"
{-
  %0 = load i32* %b, align 4                      ; <i32> [#uses=1]
-}

{----}

newReg :: StateMaybe (Int,Int) Int
newReg = SM( \(r,v) -> ((r+1,v), Just (r+1)))

newVar :: StateMaybe (Int,Int) Int
newVar = SM( \(r,v) -> ((r,v+1), Just (v+1)))

{----}

comp :: CalcI -> Env Dest -> StateMaybe (Int,Int) ([Op],Dest)

comp (Num n) env = return ([],I32 n) {- It's a constant -}

comp (Add e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- newReg
     return ( l ++ l' ++ [Add_i32 (Reg d'') d d'] , Reg d'')

comp (Sub e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- newReg
     return ( l ++ l' ++ [Sub_i32 (Reg d'') d d'] , Reg d'')

comp (Mul e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- newReg
     return ( l ++ l' ++ [Mul_i32 (Reg d'') d d'] , Reg d'')

comp (Div e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- newReg
     return ( l ++ l' ++ [Div_i32 (Reg d'') d d'] , Reg d'')

comp (Id x) env = 
     do
     d <- newReg
     case find x env of 
          Just (RegV x n) -> 
               return ([Get_var (Reg d) (RegV x n)],Reg d)
          Just _ -> raise_error
          Nothing -> raise_error

comp (Var x e e') env = 
     do
     (l,d) <- comp e env
     dv <- newVar
     (l',d') <- comp e' (assoc env x (RegV x dv))
     return (l++[New_var (RegV x dv) d]++l',d')
     

{----}

main_header = 
       "@.str = private constant [4 x i8] c\"%d\\0A\\00\", align 1 ; <[4 x i8]*> [#uses=1]\n"++
       "\n"++
       "define i32 @main() nounwind {\n"

main_footer :: Dest -> String
main_footer dest = 
       "  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0), i32 "++ (dest2Str dest) ++") nounwind ; <i32> [#uses=0]\n"++
       "  ret i32 0\n"++
       "}\n"++
       "\n"++
       "declare i32 @printf(i8*, ...) nounwind\n"

{----}

dump :: ([Op],Dest) -> String

dump (ops, d) = main_header ++ (concat (map (\o -> (op2Str o)++"\n") ops)) ++ main_footer d


compState :: CalcI -> Maybe String

compState e = let SM(f) = comp e [] in 
                  case snd (f (0,0)) of
                       Just (l,d) -> Just (dump (l,d))
                       Nothing -> Nothing 

printComp e = case compState e of
                   Just s -> putStr s
                   Nothing -> putStr "" 


{- to File -}



















