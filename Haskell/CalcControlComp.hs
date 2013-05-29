module CalcControlComp where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import MaybeMessageMonad
import StateMaybeMonad

{- Como compilar: -}
{- ghci -}
{- :load CalcControlComp -}
{- escrever comandos...ex: -}
{- printComp (Num 0) -}
{-  -}
{-  -}

{- Definition of the Abstract Syntax of the language CALCI -}

data Type =
     IntType 
  |  BoolType
  |  RefType Type
  |  UnitType
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
  | Seq CalcState CalcState 
  | Skip 

  | Equal CalcState CalcState
  | Not CalcState
  deriving (Eq,Show)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

type Env a = [(String,a)]

{- Environment manipulation functions -}

find :: Eq a => String -> Env a -> Maybe a 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a

assoc env x l = ((x,l):env)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{- Auxiliary functions -}

typeOf :: CalcState -> Type

typeOf (Num _) = IntType

typeOf (Add _ _) = IntType

typeOf (Sub _ _) = IntType

typeOf (Mul _ _) = IntType

typeOf (Div _ _) = IntType

typeOf (Id _ t) = t

typeOf (Decl x _ _ t) = t

typeOf (Equal _ _ ) = BoolType

typeOf (Not _ ) = BoolType

typeOf (Var _ t) = t

typeOf (Assign _ _ t) = t

typeOf (Deref _ t) = t

typeOf (If _ _ _ t ) = t

typeOf (While _ _) = UnitType

typeOf Skip = UnitType

typeOf (Seq _ e) = typeOf e

{- -}

toInt:: MaybeMessage Type -> MaybeMessage Type

toInt (Message s) = Message s
toInt (Result IntType) = (Result IntType)
toInt (Result _) = Message "Not an Int value."


toBoolT:: MaybeMessage Type -> MaybeMessage Type

toBoolT (Message s) = Message s
toBoolT (Result BoolType) = (Result BoolType)
toBoolT (Result _) = Message "Not a Bool value."

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{------ TypeChecker Stuff -----}

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

typecheck Skip _ = return Skip 

typecheck (If e e' e'' _) env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          e_t'' <- typecheck e'' env
          case typeOf e_t of
               BoolType | (typeOf e_t') == (typeOf e_t'') -> 
                        return (If e_t e_t' e_t'' (typeOf e_t'))
               BoolType -> Message "Type mismatch on conditional branches."
               _ -> Message "Not a Bool value in if condition." 
          
typecheck (Seq e e') env = 
          do
          e_t <- typecheck e env
          e_t'<- typecheck e' env
          return (Seq e_t e_t')

typecheck (While e e') env = 
          do 
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          case typeOf e_t of
               BoolType -> return (While e_t e_t')
               _ -> Message "Not a Bool value in while condition."


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{------ Compiler Stuff -----}

data Dest = 
    Reg Int {- Registers -}
  | I32 Int {- Constants -}
  deriving (Eq, Show)

data Label = 
     LabelT Int 
   | LabelS String
   deriving (Eq,Show)

data Op = 
    Add_i32 Dest Dest Dest
  | Sub_i32 Dest Dest Dest
  | Mul_i32 Dest Dest Dest
  | Div_i32 Dest Dest Dest
  | New_var Type Dest Dest
  | Set_var Type Dest Dest
  | Get_var Type Dest Dest
  | Free_var Dest
  | Phi Type Dest Dest Label Dest Label
  | Eq_i32 Dest Dest Dest
  | Not_i1 Dest Dest
  deriving (Eq, Show)

{----}

data Block = 
     IBlock Label [Op] Label             
     -- inconditional block
   | CBlock Label [Op] (Dest,Label,Label) 
     -- conditional block (needs condition and two labels)

type State = (Int, Int) -- for registers and labels

{----}

dest2Str :: Dest -> String

dest2Str (I32 n) = show n

dest2Str (Reg n) = "%"++(show n)  

{----}

label2Str :: Label -> String

label2Str (LabelT n) = "bb" ++ show n

label2Str (LabelS s) = s


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

op2Str (New_var (RefType _) d' d'') = 
       "  "++(dest2Str d')++" = call %struct.var_type* "++
       "@var_var_create(%struct.var_type* "++(dest2Str d'')++") nounwind ssp "

op2Str (New_var t d' d'') = 
       "  "++(dest2Str d')++" = call %struct.var_type* @int_var_create(i32 "++
       (dest2Str d'')++") nounwind ssp "


op2Str (Set_var (RefType _) d' d'') =
       "  "++"call void @var_set_var(%struct.var_type* "++(dest2Str d')++
       ", %struct.var_type* "++(dest2Str d'')++") nounwind ssp"

op2Str (Set_var t d' d'') =
       "  "++"call void @int_set_var(%struct.var_type* "++(dest2Str d')++
       ", i32 "++(dest2Str d'')++") nounwind ssp"

op2Str (Get_var (RefType _) d' d'') = 
       "  "++(dest2Str d')++" = call %struct.var_type* "++
       "@var_get_var(%struct.var_type* "++(dest2Str d'')++") nounwind ssp "

op2Str (Get_var t d' d'') = 
       "  "++(dest2Str d')++" = call i32 @int_get_var(%struct.var_type* "++
       (dest2Str d'')++") nounwind ssp "

op2Str (Free_var d) = 
       "  call void @free_var(%struct.var_type* "++(dest2Str d)++") nounwind ssp "

op2Str (Phi t d d' l' d'' l'') = "  "++dest2Str d++" = phi i32 ["++
       dest2Str d'++", %"++label2Str l'++"], ["++dest2Str d''++", %"++
       label2Str l''++"]"

op2Str (Not_i1 d d') = "  "++dest2Str d++" = xor i1 1, "++dest2Str d'

op2Str (Eq_i32 d d' d'') = "  "++dest2Str d++" = icmp eq i32 "++
       dest2Str d'++", "++dest2Str d''


{----}

newReg :: StateMaybe State Dest
newReg = SM( \(r,v) -> ((r+1,v), Just $ Reg $ r))

newLabel :: StateMaybe State Label
newLabel = SM( \(r,v) -> ((r,v+1), Just $ LabelT $ v))

{----}

comp :: CalcState -> Env Dest -> (Label,Label) -> 
        StateMaybe State ([Block],Dest,Label)

comp (Num n) env (start,end) = return ([],I32 n,start) 
     {- It's a constant, ignores the end label -}

comp (Add e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,d,m) <- comp e env (start,m) 
     (bs',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Add_i32 d'' d d'] end]], d'',end)

comp (Sub e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Sub_i32 d'' d d'] end]], d'',end)

comp (Mul e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Mul_i32 d'' d d'] end]], d'',end)

comp (Div e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Div_i32 d'' d d'] end]], d'',end)

comp (Id x t) env (start,end) = 
     case find x env of 
          Just d -> return ([], d, start)
          Nothing -> raise_error
     
comp (Decl x e e' t) env (start,end) = 
     do
     m <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',end) <- comp e' (assoc env x d) (m,end)
     return (concat [ bs, bs'], d',end) 

comp (Var e (RefType t)) env (start,end) = 
     do
     m <- newLabel     
     (bs,d,m) <- comp e env (start,m)
     d' <- newReg
     return (concat [bs, [IBlock m [New_var t d' d] end]], d', end )

comp (Var e t) env (start,end) = 
     do
     m <- newLabel     
     (bs,d,m) <- comp e env (start,m)
     d' <- newReg
     return (concat [bs, [IBlock m [New_var t d' d] end]], d', end )

comp (Deref e t) env (start,end) = 
     do
     m <- newLabel     
     (bs,d,m) <- comp e env (start,m)
     d' <- newReg
     return (concat [bs, [IBlock m [Get_var t d' d] end]], d', end )

comp (Assign e e' t) env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',m') <- comp e' env (m,m') 
     return (concat [bs, bs', [IBlock m' [Set_var t d d'] end]], d',end)

comp (Free e) env (start,end) = 
     do
     m <- newLabel     
     (bs,d,m) <- comp e env (start,m)
     return (concat [ bs, [IBlock m [Free_var d] end]], d, end) 
     {- no sense of continuing using d -}

comp (Equal e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Eq_i32 d'' d d'] end]], d'',end)

comp (Not e ) env (start,end) = 
     do
     m <- newLabel
     (bs,d,m) <- comp e env (start,m)
     d' <- newReg
     return ( concat [bs, [IBlock m [Not_i1 d' d ] end]], d',end)

comp (Seq e e') env (start,end) = 
     do
     m <- newLabel
     (bs,d,m) <- comp e env (start,m)
     (bs',d',end) <- comp e' env (m,end)
     return (concat [bs, bs'], d',end) 
     
{-
while e_c do e_b

start:
       [[e_c]] -> %d, cond
cond: 
       br i1 %d, body, end
body:
       [[e_b]] -> %d', exit
exit:
       br start
-}
comp (While e_c e_b) env (start,end) =      
     do
     l_cond <- newLabel
     l_body <- newLabel
     l_exit <- newLabel
     (bs,d, l_cond) <- comp e_c env (start,l_cond)
     (bs',d',l_exit) <- comp e_b env (l_body,l_exit)
     return (concat [bs, bs', [CBlock l_cond [] (d,l_body,end), 
                               IBlock l_exit [] start]], d',end)


{- if e then e' else e'' 

start:
        e -> %d, cond
cond: 
        br i1 %d, then, else
then:
        e'' -> %d', then_exit
then_exit:
        br exit
else:
        e'' -> %d'', else_exit
else_exit:
        br exit
exit:
        %d''' <- phi %d', %d''
        br end
-}

comp (If e_c e_t e_e t) env (start,end) = 
     do 
     l_cond <- newLabel
     l_then <- newLabel
     l_else <- newLabel
     l_then_exit <- newLabel
     l_else_exit <- newLabel
     l_exit <- newLabel
     d''' <- newReg
     (bs,d, l_cond) <- comp e_c env (start,l_cond)
     (bs',d',l_then_exit) <- comp e_t env (l_then,l_then_exit)
     (bs'',d'',l_else_exit) <- comp e_e env (l_else,l_else_exit)
     let all_bs = concat [bs,bs',bs'',
            [CBlock l_cond [] (d,l_then,l_else)],
            [IBlock l_then_exit [] l_exit],
            [IBlock l_else_exit [] l_exit],
            [IBlock l_exit [Phi t d''' d' l_then_exit d'' l_else_exit] end]] in 
                    return (all_bs, d''', end)


{----}

main_header = 
       "@.str = private constant [4 x i8] c\"%d\\0A\\00\", align 1 ; "++
       "<[4 x i8]*> [#uses=1]\n"++
       "\n"++
       "%struct.var_type = type { i32, %union.anon }\n"++
       "%union.anon = type { %struct.var_type* }\n"++
       "\n"++
       "define i32 @main() nounwind {\n"

main_footer :: Dest -> Label -> String
main_footer dest end = 
       label2Str end ++ ":\n"++
       "  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* "++
       "@.str, i64 0, i64 0), i32 "++ (dest2Str dest) ++
       ") nounwind ; <i32> [#uses=0]\n"++
       "  ret i32 0\n"++
       "}\n"++
       "\n"++
       "declare i32 @printf(i8*, ...) nounwind\n"++
       "declare %struct.var_type* @int_var_create(i32)\n"++
       "declare %struct.var_type* @var_var_create(%struct.var_type*)\n"++
       "declare i32 @int_get_var(%struct.var_type*)\n"++
       "declare %struct.var_type* @var_get_var(%struct.var_type*)\n"++
       "declare void @int_set_var(%struct.var_type*, i32)\n"++
       "declare void @var_set_var(%struct.var_type*, %struct.var_type*)\n"++
       "declare void @free_var(%struct.var_type*, %struct.var_type*)\n"

{----}

block2Str :: Block -> String

block2Str (IBlock l ops l') = 
          (label2Str l) ++ ":\n" ++ 
          (concat $ map (\o -> (op2Str o) ++ "\n") ops) ++
          "  br label %"++ (label2Str l')

block2Str (CBlock l ops (d,l',l'')) = 
          (label2Str l) ++ ":\n" ++ 
          (concat $ map (\o -> (op2Str o) ++ "\n") ops) ++
          "  br i1 "++ (dest2Str d) ++", label %"++ (label2Str l')++", label %"++ (label2Str l'')


dump :: [Block] -> Dest -> Label -> String

dump bs d end = 
     main_header ++ 
     (concat (map (\b -> (block2Str b)++"\n\n") bs)) ++ 
     main_footer d end

compState :: CalcState -> Maybe String

compState e = let SM(f) = comp e [] (LabelS "entry", LabelS "result") in 
                  case snd (f (0,0)) of
                       Just (bs,d,end) -> Just $ dump bs d end
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

        decl x = var(0) in x := !x + 1  ==>  1
-}

a = Decl "x" (Var (Num 0) None) (Assign (Id "x" None) (Add (Deref (Id "x" None) None) (Num 1)) None) None

{-

decl x = var(0) in
     While (not !x = 10) x := !x + 1
     ;
     !x
-}

a' = Decl "x" (Var (Num 0) None) (Seq (While (Not (Equal (Deref (Id "x" None) None) (Num 10))) (Assign (Id "x" None) (Add (Deref (Id "x" None) None) (Num 1)) None)) (Deref (Id "x" None) None)) None 

a_if = If (Num 0) (Add (Num 1) (Num 1)) (Add (Num 2) (Num 2)) IntType

a_if' = If (Num 0) (Num 1) (Add (Num 2) (Num 2)) IntType

fact = Decl "x" (Var (Num 0) None) 
       (Decl "s" (Var (Num 1) None) 
       (Seq 
       (While (Not (Equal (Deref (Id "x" None) None) (Num 10))) 
              (Seq 
               (Assign (Id "x" None) (Add (Deref (Id "x" None) None) (Num 1)) None)
               (Assign (Id "s" None) (Mul (Deref (Id "s" None) None) (Deref (Id "x" None) None)) None)))
       (Deref (Id "s" None) None)) None) None