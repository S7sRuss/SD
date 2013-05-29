module CalcStateComp where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import MaybeMessageMonad
import StateMaybeMonad

{- Definition of the Abstract Syntax of the language CALCI -}

data Type =
     IntType 
  |  BoolType
  |  RefType Type
  |  None
  deriving (Eq,Show)

data CalcState = 
    Num Int
  | Add CalcState CalcState
  | Sub CalcState CalcState
  | Mul CalcState CalcState
  | Div CalcState CalcState

  | Id String Type
  | Decl String CalcState CalcState Type

  | Var CalcState Type
  | Deref CalcState Type
  | Assign CalcState CalcState Type
  | Free CalcState 

  | If CalcState CalcState CalcState Type
  | While CalcState CalcState
  | Seq CalcState CalcState Type

  | Equal CalcState CalcState
  | Not CalcState
  deriving (Eq,Show)

type Env a = [(String,a)]

{- Environment manipulation functions -}

find :: Eq a => String -> Env a -> Maybe a 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a

assoc env x l = ((x,l):env)

{- -}

typeOf :: CalcState -> Type

typeOf (Num _) = IntType

typeOf (Add _ _) = IntType

typeOf (Sub _ _) = IntType

typeOf (Mul _ _) = IntType

typeOf (Div _ _) = IntType

typeOf (Id _ t) = t

typeOf (Decl x _ _ t) = t

typeOf (Equal _ _ ) = BoolType

typeOf (Var _ t) = t

typeOf (Assign _ _ t) = t

typeOf (Deref _ t) = t

{- -}


toInt:: MaybeMessage Type -> MaybeMessage Type

toInt (Message s) = Message s
toInt (Result IntType) = (Result IntType)
toInt (Result _) = Message "Not an Int value."


toBoolT:: MaybeMessage Type -> MaybeMessage Type

toBoolT (Message s) = Message s
toBoolT (Result BoolType) = (Result BoolType)
toBoolT (Result _) = Message "Not a Bool value."


typecheck :: CalcState -> Env Type -> MaybeMessage CalcState

typecheck (Id x _) env = 
          case find x env of
               Just t -> Result (Id x t)
               Nothing -> Message ("Identifier "++x++ " not found.")

typecheck (Decl x e e' _) env =
          do
          e_t <- typecheck e env
          e_s <- typecheck e' (assoc env x (typeOf e_t))
          return (Decl x e_t e_s (typeOf e_s))

typecheck (Num n) env = return (Num n)

typecheck (Add e e') env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          if typeOf e_t == typeOf e_t' && typeOf e_t == IntType 
          then return (Add e_t e_t')
          else Message "Wrong type in addition."

typecheck (Sub e e') env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          if typeOf e_t == typeOf e_t' && typeOf e_t == IntType 
          then return (Sub e_t e_t')
          else Message "Wrong type in subtraction."

typecheck (Mul e e') env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          if typeOf e_t == typeOf e_t' && typeOf e_t == IntType 
          then return (Mul e_t e_t')
          else Message "Wrong type in multiplication."

typecheck (Div e e') env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          if typeOf e_t == typeOf e_t' && typeOf e_t == IntType 
          then return (Div e_t e_t')
          else Message "Wrong type in division."

typecheck (Equal e e') env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          if typeOf e_t == typeOf e_t' then return (Equal e_t e_t')
          else Message "Comparison between values of non-equal types."


typecheck (Not e) env = 
          do
          e_t <- typecheck e env
          if typeOf e_t == BoolType then return (Not e_t)
          else Message "Expecting a boolean expression."
          
typecheck (Var e _) env = 
          do 
          e_t <- typecheck e env
          return (Var e_t (RefType (typeOf e_t)));

typecheck (Deref e _) env = 
          do 
          e_t <- typecheck e env
          case typeOf e_t of
             RefType t' -> return (Deref e_t t')
             _ -> Message "Dereferencing a non-reference value."

typecheck (Assign e e' _) env = 
          do
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          case typeOf e_t of
             RefType t'' | t'' == typeOf e_t' -> return (Assign e_t e_t' t'') 
             RefType _ -> Message "Type mismatch on assignment."
             _ -> Message "Assigning a non-reference value."


{------ Compiler Stuff -----}

data Dest = 
    Reg Int {- Registers -}
  | I32 Int {- Constants -}
  deriving (Eq, Show)

data Op = 
    Add_i32 Dest Dest Dest
  | Sub_i32 Dest Dest Dest
  | Mul_i32 Dest Dest Dest
  | Div_i32 Dest Dest Dest
  | New_var Type Dest Dest
  | Set_var Type Dest Dest
  | Get_var Type Dest Dest
  | Free_var Dest
  deriving (Eq, Show)

{----}

dest2Str :: Dest -> String

dest2Str (I32 n) = show n

dest2Str (Reg n) = "%"++(show n)

{----}

op2Str :: Op -> String

op2Str (Add_i32 d d' d'') = 
       "  "++(dest2Str d)++" = add i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (Sub_i32 d d' d'') = 
       "  "++(dest2Str d)++" = sub i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (Mul_i32 d d' d'') = 
       "  "++(dest2Str d)++" = mul i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (Div_i32 d d' d'') =
       "  "++(dest2Str d)++" = sdiv i32 "++(dest2Str d')++", "++(dest2Str d'')

op2Str (New_var IntType d' d'') = 
       "  "++(dest2Str d')++" = call %struct.var_type* @int_var_create(i32 "++(dest2Str d'')++") nounwind ssp "

op2Str (New_var (RefType _) d' d'') = 
       "  "++(dest2Str d')++" = call %struct.var_type* @var_var_create(%struct.var_type* "++(dest2Str d'')++") nounwind ssp "


op2Str (Set_var IntType d' d'') =
       "  "++"call void @int_set_var(%struct.var_type* "++(dest2Str d')++", i32 "++(dest2Str d'')++") nounwind ssp"

op2Str (Set_var (RefType _) d' d'') =
       "  "++"call void @var_set_var(%struct.var_type* "++(dest2Str d')++", %struct.var_type* "++(dest2Str d'')++") nounwind ssp"

op2Str (Get_var IntType d' d'') = 
       "  "++(dest2Str d')++" = call i32 @int_get_var(%struct.var_type* "++(dest2Str d'')++") nounwind ssp "

op2Str (Get_var (RefType _) d' d'') = 
       "  "++(dest2Str d')++" = call %struct.var_type* @int_get_var(%struct.var_type* "++(dest2Str d'')++") nounwind ssp "


{-

op2Str (Free_var d ) = call free

-}

{----}

comp :: CalcState -> Env Dest -> StateMaybe Int ([Op],Dest)


comp (Num n) env = return ([],I32 n) {- It's a constant -}

comp (Add e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- inc
     return ( l ++ l' ++ [Add_i32 (Reg d'') d d'] , Reg d'')

comp (Sub e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- inc
     return ( l ++ l' ++ [Sub_i32 (Reg d'') d d'] , Reg d'')

comp (Mul e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- inc
     return ( l ++ l' ++ [Mul_i32 (Reg d'') d d'] , Reg d'')

comp (Div e e') env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' env
     d'' <- inc
     return ( l ++ l' ++ [Div_i32 (Reg d'') d d'] , Reg d'')

comp (Id x t) env = 
     case find x env of 
          Just d -> return ([], d)
          Nothing -> raise_error
     
comp (Decl x e e' t) env = 
     do
     (l,d) <- comp e env
     (l',d') <- comp e' (assoc env x d)
     return (l ++ l', d') 

comp (Var e (RefType t)) env = 
     do
     (l,d) <- comp e env
     d' <- inc
     return (l ++ [New_var t (Reg d') d], Reg d')     

comp (Deref e t) env = 
     do
     (l,d) <- comp e env
     d' <- inc
     return (l ++ [Get_var t (Reg d') d], Reg d')     

comp (Assign e e' t) env = 
     do 
     (l,d) <- comp e env
     (l',d') <- comp e' env
     return (l ++ l' ++ [Set_var t d d'], d')

comp (Free e) env = 
     do 
     (l,d) <- comp e env
     return (l ++ [Free_var d], d) {- no sense of continuing using d -}


{----}

main_header = 
       "@.str = private constant [4 x i8] c\"%d\\0A\\00\", align 1 ; <[4 x i8]*> [#uses=1]\n"++
       "\n"++
       "%struct.var_type = type { i32, %union.anon }\n"++
       "%union.anon = type { %struct.var_type* }\n"++
       "\n"++
       "define i32 @main() nounwind {\n"

main_footer :: Dest -> String
main_footer dest = 
       "  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0), i32 "++ (dest2Str dest) ++") nounwind ; <i32> [#uses=0]\n"++
       "  ret i32 0\n"++
       "}\n"++
       "\n"++
       "declare i32 @printf(i8*, ...) nounwind\n"++
       "declare %struct.var_type* @int_var_create(i32)\n"++
       "declare %struct.var_type* @var_var_create(%struct.var_type*)\n"++
       "declare i32 @int_get_var(%struct.var_type*)\n"++
       "declare %struct.var_type* @var_get_var(%struct.var_type*)\n"++
       "declare void @int_set_var(%struct.var_type*, i32)\n"++
       "declare void @var_set_var(%struct.var_type*, %struct.var_type*)\n"

{----}

dump :: ([Op],Dest) -> String

dump (ops, d) = main_header ++ (concat (map (\o -> (op2Str o)++"\n") ops)) ++ main_footer d


compState :: CalcState -> Maybe String

compState e = let SM(f) = comp e [] in 
                  case snd (f 0) of
                       Just (l,d) -> Just (dump (l,d))
                       Nothing -> Nothing 

printComp e = case compState e of
                   Just s -> putStr s
                   Nothing -> putStr "" 


{- to File -}

saveComp e = 
         case typecheck e [] of
              Result e -> 
                       case compState e of
                           Just s -> writeFile "a.s" s
                           Nothing -> return ()
              Message s -> putStr s
         

{-

        decl x = var(0) in x := 1  ==>  1
-}

a = Decl "x" (Var (Num 0) (RefType IntType)) (Assign (Id "x" (RefType IntType)) (Num 1) IntType) IntType

a' = Decl "x" (Var (Num 0) None) (Assign (Id "x" None) (Num 1) None) None