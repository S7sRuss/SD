module CoreCompiler where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

import MaybeMessageMonad
import StateMaybeMonad
import System.Cmd (system)
import System.Environment ( getArgs )
import qualified Data.List as List
import GHC.IO.Exception (ExitCode)
import Foreign (sizeOf)
import Text.StringTemplate

{- Definition of the Abstract Syntax of the language CALCI -}

data Type =
     IntType 
  |  BoolType
  |  RefType Type
  |  UnitType
  |  FunType Type Type
  |  None
  deriving (Eq,Show)

data Core = 
    Num Int
  | Add Core Core
  | Sub Core Core
  | Mul Core Core
  | Div Core Core

  | Id String Type
  | Decl String Core Core Type

  | Var Core Type
  | Deref Core Type
  | Assign Core Core Type
  | Free Core 

  | If Core Core Core Type
  | While Core Core
  | Seq Core Core 
  | Skip 

  | Equal Core Core
  | Not Core

  | Fun String Type Core Type
  | Call Core Core Type
  deriving (Eq,Show)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{- Environment manipulation functions -}

type Env a = [(String,a)]

find :: Eq a => String -> Env a -> Maybe a 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a

assoc env x l = ((x,l):env)

assocLast :: Env a -> String -> a -> Env a

assocLast env x l = env ++ [(x,l)]

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{- Auxiliary functions -}

typeOf :: Core -> Type

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

typeOf (Fun _ _ _ t) = t

typeOf (Call _ _ t) = t

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

{------ Syntax Stuff -----}

free :: Core -> [(String,Type)]

free (Num n)       = []
free (Add e e')    = List.union (free e) (free e')
free (Sub e e')    = List.union (free e) (free e')
free (Mul e e')    = List.union (free e) (free e')
free (Div e e')    = List.union (free e) (free e')

free (Id x t)        = [(x,t)]
free (Decl x e e' _) = List.union (free e) [(y,t) | (y,t) <- free e', x /= y]

free (Fun x _ e _)    =  [(y,t) | (y,t) <- free e, x /= y]
free (Call e e' _)   = List.union (free e) (free e')

free (Var e _ ) = free e
free (Deref e _ ) = free e
free (Assign e e' _ ) = List.union (free e) (free e')
free (Free e) = free e

free (If e e' e'' _ ) = List.union (List.union (free e) (free e')) (free e'')
free (While e e') = List.union (free e) (free e')
free (Seq e e') = List.union (free e) (free e')
free (Skip) = []

free (Equal e e')    = List.union (free e) (free e')
free (Not e)    = (free e) 

{------ TypeChecker Stuff -----}

typecheck :: Core -> Env Type -> MaybeMessage Core

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

typecheck (Fun x t_p e _) env = 
          do
          e_t <- typecheck e (assoc env x t_p)
          return (Fun x t_p e_t (FunType t_p (typeOf e_t)))

typecheck (Call e e' _) env = 
          do
          e_t <- typecheck e env
          e_t' <- typecheck e' env
          case typeOf e_t of 
               FunType t_p t_r | t_p == typeOf e_t' -> return (Call e_t e_t' t_r)
               _ -> Message "Not a function type value."

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

{------ Compiler Stuff -----}

data Dest = 
    Reg Int {- Registers -}
  | I32 Int {- Constants -}
  | REnv String Type Int {- Identifiers in the closure environmen -}
  deriving (Eq, Show)

data Label = 
     LabelT Int 
   | LabelS String
   deriving (Eq,Show)

type FunLabel = Int 

entryresult = (LabelS "entry", LabelS "result")

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
  | Call_type Type Dest Dest Dest
  | Get_env Type Dest Dest Dest String Int
  | Create_fn Type Dest Dest FunLabel [(Dest,Type)]
  deriving (Eq, Show)

{----}

data Block = 
     IBlock Label [Op] Label             
     -- inconditional block
   | CBlock Label [Op] (Dest,Label,Label) 
     -- conditional block (needs condition and two labels)
   deriving (Eq,Show)

type State = ([Int], Int, Int) -- for levels of registers, labels, and function names

data Function = 
     Closure FunLabel Type [Block] Dest Label (Env Dest) Int
     deriving (Eq,Show)

{----}

emptyState = ([0],0,0)

newReg :: StateMaybe State Dest
newReg = SM( \(r:rs,l,f) -> ((r+1:rs,l,f), Just (Reg r)))

newLabel :: StateMaybe State Label
newLabel = SM( \(r,l,f) -> ((r,l+1,f), Just $ LabelT l))

newFunName :: StateMaybe State FunLabel
newFunName = SM (\(r,l,f) -> ((r,l,f+1), Just f)) 

beginScope :: StateMaybe State Dest
beginScope = SM( \(rs,l,f) -> ((1:rs,l,f), Just (Reg 0)))

endScope :: StateMaybe State Dest
endScope = SM( \(r:rs,l,f) -> ((rs,l,f), Just (Reg r)))

wSize = sizeOf (undefined :: Int)

alignOf IntType = 4
alignOf (FunType _ _) = wSize
alignOf (RefType _) = wSize

nextAlign :: Type -> Int -> (Int,Int)
nextAlign IntType n                           = (n,n+4)            -- i32
nextAlign (FunType _ _) n | rem n wSize == 0  = (n,n+wSize)
nextAlign (FunType _ _) n                     = (n+4,n+4+wSize)    -- it must be a multiple of 4

nextAlign (RefType _) n | rem n wSize == 0    = (n,n+wSize)
nextAlign (RefType _) n                       = (n+4,n+4+wSize)    -- it must be a multiple of 4

assocAlign :: (Env Dest, Int) -> (String,Type) -> (Env Dest, Int)
assocAlign = \(env,start) -> 
             \(y,t) -> 
             let (this,next) = nextAlign t start in 
                 (assocLast env y $ REnv y t this, next)


{----}

dest2Str :: Dest -> String

dest2Str (I32 n) = show n

dest2Str (Reg n) = "%"++(show n)  

{----}

label2Str :: Label -> String

label2Str (LabelT n) = "bb" ++ show n

label2Str (LabelS s) = s


{----}

type2Str :: Type -> String

type2Str IntType = "_int" 

type2Str BoolType = "_bool"

type2Str (RefType t) = "_ref"++type2Str t

type2Str (FunType t t') = "_fun"++type2Str t++type2Str t'

{----} 

type2LLVM :: Type -> String

type2LLVM IntType = "i32"

type2LLVM BoolType = "i1"

type2LLVM (RefType _) = "%struct.var_type*"

type2LLVM (FunType _ _) = "%struct.closure_type*"

{----}

parList :: Env Dest -> String

parList env = List.intercalate ", " $ [ type2LLVM t++" %"++x | (x,REnv y t n) <- env ] 

argList :: [(Dest,Type)] -> String

argList args = List.intercalate ", " $ map (\(d,t)-> (type2LLVM t++" "++dest2Str d)) args

{----}

fill s l = render $ 
           setManyAttrib (zip [show n | n <- [0..length l]] l) $ 
           (newSTMP s :: StringTemplate String)

op2Str :: Op -> String

op2Str (Add_i32 d d' d'') = 
  fill "  $0$ = add i32 $1$, $2$" [dest2Str d,dest2Str d',dest2Str d'']

op2Str (Sub_i32 d d' d'') = 
  fill "  $0$ = sub i32 $1$, $2$" [dest2Str d,dest2Str d',dest2Str d'']

op2Str (Mul_i32 d d' d'') = 
  fill "  $0$ = mul i32 $1$, $2$" [dest2Str d,dest2Str d',dest2Str d'']

op2Str (Div_i32 d d' d'') =
  fill "  $0$ = sdiv i32 $1$, $2$" [dest2Str d,dest2Str d',dest2Str d'']

op2Str (New_var IntType d d') = 
  fill "  $0$ = call %struct.var_type* @int_var_create(i32 $1$) nounwind ssp " 
       [dest2Str d, dest2Str d']

op2Str (New_var (RefType _) d d') = 
  fill "  $0$ = call %struct.var_type* @var_var_create(%struct.var_type* $1$) nounwind ssp "
       [dest2Str d, dest2Str d']

op2Str (Set_var IntType d d') =
  fill "  call void @int_set_var(%struct.var_type* $0$, i32 $1$) nounwind ssp"
       [dest2Str d, dest2Str d']

op2Str (Set_var (RefType _) d d') =
  fill "  call void @var_set_var(%struct.var_type* $0$, %struct.var_type* $0$) nounwind ssp"
       [dest2Str d, dest2Str d']

op2Str (Get_var IntType d d') = 
  fill "  $0$ = call i32 @int_get_var(%struct.var_type* $1$) nounwind ssp "
       [dest2Str d, dest2Str d']

op2Str (Get_var (RefType _) d d') = 
  fill "  $0$ = call %struct.var_type* @var_get_var(%struct.var_type* $1$) nounwind ssp "
       [dest2Str d, dest2Str d']

op2Str (Free_var d) = 
  fill "  call void @free_var(%struct.var_type* $0$) nounwind ssp " [dest2Str d]

op2Str (Phi IntType d d' l' d'' l'') = 
  fill "  $0$ = phi i32 [$1$, %$2$], [$3$, %$4$]"
       [dest2Str d, dest2Str d', label2Str l', dest2Str d'', label2Str l'']

op2Str (Not_i1 d d') = 
  fill "  $0$ = xor i1 1, $1$" [dest2Str d, dest2Str d']

op2Str (Eq_i32 d d' d'') = 
  fill "  $0$ = icmp eq i32 $1$, $2$"  [dest2Str d, dest2Str d', dest2Str d'']

op2Str (Call_type (FunType t t') d d' d'') = 
  fill " $0$ = call $1$ @closure_call$2$(%struct.closure_type* $3$, $4$ $5$) nounwind ssp"
    [dest2Str d, type2LLVM t', type2Str (FunType t t'), dest2Str d', type2LLVM t, dest2Str d'']

op2Str (Get_env t d d' d'' y n) = 
  fill 
    ("; get $0$ align $1$\n"++
     "  $2$ = getelementptr inbounds i8* %env, i32 $1$\n"++
     "  $3$ = bitcast i8* $2$ to $5$*\n"++
     "  $4$ = load $5$* $3$, align $6$\n" )
    [y, show n, dest2Str d, dest2Str d', dest2Str d'', type2LLVM t, show (alignOf t) ]

op2Str (Create_fn (FunType t t') d d' fn args) = 
  fill
    (" $0$ = call i8* @create_env_f$1$($2$) nounwind ssp\n"++
     " $3$ = call %struct.closure_type* @closure_create(i8* $4$,"++
     " void ()* bitcast ($5$ ($6$, i8*)* @f$1$ to void ()*)) nounwind")
     [ dest2Str d', show fn, argList args, dest2Str d, dest2Str d', type2LLVM t', type2LLVM t]

{----}

comp :: Core -> Env Dest -> (Label,Label) -> 
        StateMaybe State ([Block],[Function],[Type],Dest,Label)

comp (Num n) env (start,end) = return ([],[],[],I32 n,start) 
     {- It's a constant, ignores the end label -}

comp (Add e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m) 
     (bs',fs',ts',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Add_i32 d'' d d'] end]], 
              fs ++ fs', List.union ts ts', d'',end)

comp (Sub e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m) 
     (bs',fs',ts',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Sub_i32 d'' d d'] end]], 
              fs ++ fs', List.union ts ts', d'',end)

comp (Mul e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m) 
     (bs',fs',ts',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Mul_i32 d'' d d'] end]], 
              fs ++ fs', List.union ts ts', d'',end)

comp (Div e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m) 
     (bs',fs',ts',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Div_i32 d'' d d'] end]], 
              fs ++ fs', List.union ts ts', d'',end)

comp (Id x t) env (start,end) = 
     case find x env of 
          Just (REnv y _ n) -> 
                do
                d <- newReg
                d' <- newReg
                d'' <- newReg
                return ([IBlock start [Get_env t d d' d'' y n] end], [], [], d'', end)
          Just d -> return ([], [], [], d, start)
          Nothing -> raise_error
     
comp (Decl x e e' t) env (start,end) = 
     do
     m <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m)
     (bs',fs',ts',d',end) <- comp e' (assoc env x d) (m,end)
     return (concat [ bs, bs'], fs ++ fs', List.union ts ts', d',end) 

comp (Var e (RefType t)) env (start,end) = 
     do
     m <- newLabel     
     (bs,fs,ts,d,m) <- comp e env (start,m)
     d' <- newReg
     return (concat [bs, [IBlock m [New_var t d' d] end]], fs, ts, d', end )

comp (Deref e t) env (start,end) = 
     do
     m <- newLabel     
     (bs,fs,ts,d,m) <- comp e env (start,m)
     d' <- newReg
     return (concat [bs, [IBlock m [Get_var t d' d] end]], fs, ts, d', end )

comp (Assign e e' t) env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m)
     (bs',fs',ts',d',m') <- comp e' env (m,m') 
     return (concat [bs, bs', [IBlock m' [Set_var t d d'] end]], 
                    fs ++ fs', List.union ts ts', d',end)

comp (Free e) env (start,end) = 
     do
     m <- newLabel     
     (bs,fs,ts,d,m) <- comp e env (start,m)
     return (concat [ bs, [IBlock m [Free_var d] end]], fs, ts, d, end) 
     {- no sense of continuing using d -}

comp (Equal e e') env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m)
     (bs',fs',ts',d',m') <- comp e' env (m,m') 
     d'' <- newReg
     return ( concat [ bs, bs', [IBlock m' [Eq_i32 d'' d d'] end]], 
              fs ++ fs', List.union ts ts', d'',end)

comp (Not e ) env (start,end) = 
     do
     m <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m)
     d' <- newReg
     return ( concat [bs, [IBlock m [Not_i1 d' d ] end]], fs, ts, d',end)

comp (Seq e e') env (start,end) = 
     do
     m <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m)
     (bs',fs',ts',d',end) <- comp e' env (m,end)
     return (concat [bs, bs'], fs ++ fs', List.union ts ts', d',end) 
     
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
     (bs,fs,ts,d, l_cond) <- comp e_c env (start,l_cond)
     (bs',fs',ts', d',l_exit) <- comp e_b env (l_body,l_exit)
     return (concat [bs, bs', [CBlock l_cond [] (d,l_body,end), 
                               IBlock l_exit [] start]], fs ++ fs', 
             List.union ts ts', d',end)


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
     (bs,fs,ts,d, l_cond) <- comp e_c env (start,l_cond)
     (bs',fs',ts',d',l_then_exit) <- comp e_t env (l_then,l_then_exit)
     (bs'',fs'',ts'',d'',l_else_exit) <- comp e_e env (l_else,l_else_exit)
     d''' <- newReg
     let all_bs = concat [bs,bs',bs'',
                  [CBlock l_cond [] (d,l_then,l_else)],
                  [IBlock l_then_exit [] l_exit],
                  [IBlock l_else_exit [] l_exit],
                  [IBlock l_exit [Phi t d''' d' l_then_exit d'' l_else_exit] end]] in 
                     return (all_bs, 
                             fs ++ fs' ++ fs'', 
                             List.union (List.union ts ts') ts'', d''', end)

comp (Fun x _ e t) env (start,end) = 
     -- collect the non-local names in the closure
     let envnames = [ (y,t) | (y,t) <- free e, y /= x ] in  
     -- build and align a record
     let (nenv_free,mallocSize) = foldl assocAlign ([],0) envnames in 
       -- build env
       do
       fn <- newFunName   
       -- fresh static name for the new function
       d' <- beginScope
       (bs, fns, fntypes, d''', end') <- 
         -- parameter x gets the register %0 (d')
         let nenv_par = assoc nenv_free x d' in 
         -- nenv_par contains the free names and parameter (x)
         -- compiling the function body
         comp e nenv_par entryresult          
       endScope
       d <- newReg   -- to store the environment
       d'' <- newReg -- to store the closure
       let args = [(d,t) | (x,t) <- envnames, 
                   let jd = find x env, Just d <- [jd]] in
         return ([IBlock start [Create_fn t d'' d fn args] end], 
                 (Closure fn t bs d''' end' nenv_free mallocSize):fns, 
                 List.union [t] fntypes, d'', end) 

{-

start:
        [[e]] -> d,m
m: 
        [[e']] -> d',m'
m':
        call t d d'
        br end
-}
comp (Call e e' t) env (start,end) = 
     do
     m <- newLabel
     m' <- newLabel
     (bs,fs,ts,d,m) <- comp e env (start,m)
     (bs',fs',ts',d',m') <- comp e' env (m,m')
     d'' <- newReg
     return (concat [bs, bs', 
                    [IBlock m' [Call_type (typeOf e) d'' d d'] end]], 
             fs ++ fs', List.union ts ts', d'',end)

{----}

format IntType = "%d"

format (FunType _ _) = "%p"

format (RefType _) = "%p"

format BoolType = "%d"

format UnitType = "unit"

file_header t = 
       "@.str = private constant [4 x i8] c\""++format t++"\\0A\\00\", align 1 ; "++
       "<[4 x i8]*> [#uses=1]\n"++
       "\n"++
       "%struct.var_type = type { i32, %union.anon }\n"++
       "%union.anon = type { %struct.var_type* }\n"++
       "%struct.closure_type = type { i8*, void ()* }\n"++
       "\n"

main_header = "define i32 @main() nounwind {\n"

main_footer t dest end = 
       label2Str end ++ ":\n"++
       "  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* "++
       "@.str, i64 0, i64 0), "++ type2LLVM t++" "++ (dest2Str dest) ++
       ") nounwind\n"++
       "  ret i32 0\n"++
       "}\n"++
       "\n"++
       "declare i32 @printf(i8*, ...) nounwind\n"++
       "declare noalias i8* @malloc(i64) nounwind\n"++
       "declare %struct.var_type* @int_var_create(i32)\n"++
       "declare %struct.var_type* @var_var_create(%struct.var_type*)\n"++
       "declare i32 @int_get_var(%struct.var_type*)\n"++
       "declare %struct.var_type* @var_get_var(%struct.var_type*)\n"++
       "declare void @int_set_var(%struct.var_type*, i32)\n"++
       "declare void @var_set_var(%struct.var_type*, %struct.var_type*)\n"++
       "declare void @free_var(%struct.var_type*, %struct.var_type*)\n"++
       "declare %struct.closure_type* @closure_create(i8*, void ()*)\n"

{----}

block2Str :: Block -> String

block2Str (IBlock l ops l') = 
          (label2Str l) ++ ":\n" ++ 
          (concat $ map (\o -> (op2Str o) ++ "\n") ops) ++
          "  br label %"++ (label2Str l')

block2Str (CBlock l ops (d,l',l'')) = 
          (label2Str l) ++ ":\n" ++ 
          (concat $ map (\o -> (op2Str o) ++ "\n") ops) ++
          "  br i1 "++ (dest2Str d) ++", label %"++ (label2Str l')++
          ", label %"++ (label2Str l'')

{- -}

dumpBlocks :: [Block] -> String
dumpBlocks bs = concat (map (\b -> (block2Str b)++"\n\n") bs)

dump_type :: String -> Type -> String
dump_type s (FunType t t') = 
  s++"define "++type2LLVM t'++
  " @closure_call"++type2Str (FunType t t')++"(%struct.closure_type* %closure, " ++ 
  type2LLVM t++ " %x) {\n"++
  "entry:\n"++
  "  %0 = getelementptr inbounds %struct.closure_type* %closure, i64 0, i32 1\n"++
  "  %1 = load void ()** %0, align 8\n"++
  "  %2 = bitcast void ()* %1 to "++type2LLVM t'++" ("++type2LLVM t++", i8*)*\n"++
  "  %3 = getelementptr inbounds %struct.closure_type* %closure, i64 0, i32 0\n"++
  "  %4 = load i8** %3, align 8\n"++
  "  %5 = tail call "++type2LLVM t'++" %2("++type2LLVM t++" %x, i8* %4) nounwind\n"++
  "  ret "++type2LLVM t'++" %5\n"++
  "}\n\n"

dump_types :: [Type] -> String
dump_types ts = 
           "; ---- Closure Types section -----\n"++
           (foldl dump_type "" ts)

functionEnvironment :: FunLabel -> (Env Dest) -> String
functionEnvironment fn env = 
  "%struct.env_f"++show fn++" = type { "++
  List.intercalate ", " [type2LLVM t | (x,REnv _ t n) <- env]++ 
  " }\n\n"

storeIdEnv (r,s) (_,(REnv x t n)) = 
  let d  = 2*r - 1 in
  let d' = 2*r in
  let s' = "  ; %0+"++show n++" = "++type2LLVM t++" %"++x++"\n"++
           "  %"++show d++" = getelementptr inbounds i8* %0, i32 "++show n++"\n"++
           "  %"++show d'++" = bitcast i8* %"++show d++" to "++type2LLVM t++"*\n"++
           "  store "++type2LLVM t++" %"++x++", "++type2LLVM t++"* %"++show d'++", align "++show (alignOf t)++"\n"
  in (r+1,s++s')


createEnvFunction fn env mallocSize = 
  "define i8* @create_env_f"++show fn++"("++
  parList env++
  ") {\n"++
  "entry:\n"++
  "  %0 = tail call noalias i8* @malloc(i64 "++show mallocSize++") nounwind\n"++
  snd (foldl storeIdEnv (1,"") env)++
  "  ret i8* %0\n"++
  "} ; TODO\n\n"

functionHeader :: Int -> Type -> String
functionHeader fn (FunType t t') = 
  "; f"++show fn++": "++show (FunType t t') ++ "\n"++
  "define "++type2LLVM t'++" @f"++show fn++
  "("++type2LLVM t++" %x, i8* %env) nounwind {\n"++
  "start:\n"++
  "  %0 = bitcast "++type2LLVM t++" %x to "++type2LLVM t++"\n"++
  "  br label %entry\n\n"

functionFooter :: Label -> Type -> Dest -> String
functionFooter end (FunType t t') d = 
  label2Str end ++ ":\n  ret " ++ type2LLVM t' ++ " " ++ dest2Str d ++"\n}\n\n"

dump_function :: Function -> String
dump_function (Closure fn t bs d end env mallocSize) =
  "; ---- begin f"++show fn++" -----\n"++ 
  functionEnvironment fn env++ 
  createEnvFunction fn env mallocSize++
  functionHeader fn t++
  dumpBlocks bs++
  functionFooter end t d++
  "; ---- end f"++show fn++" -----\n"


dump_functions :: [Function] -> String
dump_functions fs =
    "; ---- Closures section -----\n\n"++ 
    (foldr (\f -> \s -> s++dump_function f++"\n") "" fs)++ "\n"

{- -}

dump :: Type -> [Block] -> [Function] -> [Type] -> Dest -> Label -> String
dump t bs fs ts d end = 
     file_header t ++ 
     dump_types ts ++
     dump_functions fs ++
     main_header ++ 
     dumpBlocks bs++
     main_footer t d end

compState :: Core -> Maybe String
compState e = let SM(f) = comp e [] entryresult in 
                  case snd (f emptyState) of
                       Just (bs,fs,ts,d,end) -> Just $ dump (typeOf e) bs fs ts d end
                       Nothing -> Nothing 

printComp e = case compState e of
                   Just s -> putStr s
                   Nothing -> putStr "" 


{- to File -}

data Args =
     Source String
   | AsmFile String
   | Output String
   | Assembling
   | Compiling
   deriving (Eq,Show)

default_asmfile = "a.s"
default_bsfile = "a.s.bc"
default_execfile = "a.out"
llvmpath = "/usr/local/llvm/bin/"

-- 

parseArgs :: [String] -> [Args]

parseArgs [] = []

parseArgs ("-c":args) = Compiling : parseArgs args

parseArgs ("-s":args) = Assembling : parseArgs args

parseArgs ("-o":fn:args) = Output fn : parseArgs args

parseArgs (fn:args) = Source fn : parseArgs args

-- 

asmfile args = if elem Assembling args then 
               case [s | Output s <- args] of
                    [] -> default_asmfile
                    s:ss -> s
               else default_asmfile

compile asmfile e = 
         case typecheck e [] of
              Result e -> 
                case compState e of
                     Just s -> writeFile asmfile s
                     Nothing -> putStr "Error compiling."
              Message s -> putStr s
         
compileandlink args e = 
               let parsedArgs = parseArgs args in
               do 
               compile (asmfile parsedArgs) e                    -- compile
               system $ llvmpath++"llvm-as "++asmfile parsedArgs  -- assemble
               system $ llvmpath++"llvm-ld "++asmfile parsedArgs++".bc runtime.s.bc" -- link
               
run = 
    do 
    system "./a.out"               

main :: IO ExitCode

main = 
     do 
     args <- getArgs
     compileandlink args a' -- connect with parser :)


--        decl x = var(0) in x := !x + 1  ==>  1

a = Decl "x" (Var (Num 0) None) 
             (Assign (Id "x" None) (Add (Deref (Id "x" None) None) (Num 1)) None) None


{-

decl x = var(0) in
     While (not !x = 10) x := !x + 1
     ;
     !x
-}

a' = Decl "x" (Var (Num 0) None) 
              (Seq (While (Not (Equal (Deref (Id "x" None) None) (Num 10))) 
                          (Assign (Id "x" None) (Add (Deref (Id "x" None) None) (Num 1)) None)) 
                   (Deref (Id "x" None) None)) None 

a_if = If (Equal (Num 0) (Num 0)) (Add (Num 1) (Num 1)) (Add (Num 2) (Num 2)) IntType

a_if' = If (Not (Equal (Num 0) (Num 0))) (Add (Num 1) (Num 1)) (Add (Num 2) (Num 2)) IntType

a_if'' = If (Equal (Num 0) (Num 0)) (Num 1) (Add (Num 2) (Num 2)) IntType

--

fact = Decl "x" (Var (Num 0) None) 
       (Decl "s" (Var (Num 1) None) 
       (Seq 
       (While (Not (Equal (Deref (Id "x" None) None) (Num 10))) 
              (Seq 
               (Assign (Id "x" None) (Add (Deref (Id "x" None) None) (Num 1)) None)
               (Assign (Id "s" None) (Mul (Deref (Id "s" None) None) 
                                          (Deref (Id "x" None) None)) None)))
       (Deref (Id "s" None) None)) None) None

-- decl f = fun x -> x+x+1 in f 1

f = Decl "f" (Fun "x" IntType (Add (Id "x" None) 
                              (Add (Id "x" None) (Num 1))) None) 
             (Call (Id "f" None) (Num 1) None) None

-- decl y = 0 in decl f = fun x -> x+x+y in f 1 

f' = Decl "y" (Num 3) 
     (Decl "f" 
       (Fun "x" IntType (Add (Id "x" None) (Add (Id "x" None) (Id "y" None))) None) 
       (Call (Id "f" None) (Num 1) None) None) None

-- decl y = 1 in decl z = 2 in decl f = fun x -> x+x+y in f 1 

f'' = Decl "y" (Num 1)
      (Decl "z" (Num 2)
     (Decl "f" 
       (Fun "x" IntType (Add (Id "x" None) (Add (Id "z" None) (Id "y" None))) None) 
       (Call (Id "f" None) (Num 1) None) None) None) None

-- decl f = fun y -> fun x -> x+y in f 0 1 

h = Decl "f" 
    (Fun "y" IntType (Fun "x" IntType (Add (Id "x" None) (Id "y" None)) None) None)
    (Call (Call (Id "f" None) (Num 0) None) (Num 1) None) None

-- decl f = fun y -> fun x -> x+y in decl g = f 2 in g 3 

h' = Decl "f" 
     (Fun "y" IntType (Fun "x" IntType (Add (Id "x" None) (Id "y" None)) None) None)
     (Decl "g" (Call (Id "f" None) (Num 2) None)    
               (Call (Id "g" None) (Num 3) None) None) None

-- decl g = fun x -> x+1 in g 1

g = Decl "g" (Fun "x" IntType (Add (Id "x" None) (Num 1)) None) (Call (Id "g" None) (Num 1) None) None

-- decl w = 3 in decl y = 1 in decl g = fun x -> x + y in (fun z -> y + g (z+w)) 1

g' = Decl "w" (Num 3)
     (Decl "y" 
       (Num 1)
       (Decl "g" 
       (Fun "x" IntType (Add (Id "x" None) (Id "y" None)) None)
       (Call 
         (Fun "z" IntType (Add (Id "y" None) 
                               (Call (Id "g" None) 
                                     (Add (Id "z" None) (Id "w" None)) None)) None) 
         (Num 1) None) None)
       None) None

-- decl f = fun x -> var (x) in decl v = f 1 in v := 2; !v

v = Decl "f" 
         (Fun "x" IntType (Var (Id "x" None) None) None)
         (Decl "v" (Call (Id "f" None) (Num 1) None)
                   (Seq (Assign (Id "v" None) (Num 2) None)
                        (Deref (Id "v" None) None)) None) None

-- decl f = fun x -> x+1 in (fun f -> f 1) (f)

ff = Decl "f" (Fun "x" IntType (Add (Id "x" None) (Num 1)) None)
              (Call 
              (Fun "f" (FunType IntType IntType) 
                       (Call (Id "f" None) (Num 1) None) None)
              (Id "f" None) None) None
