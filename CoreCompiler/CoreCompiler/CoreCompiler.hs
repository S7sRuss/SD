module CoreCompiler where

import Foreign (sizeOf)
import qualified Data.List as List

import StateMaybeMonad

import Syntax
import Env

import TypeSystem

data Dest = 
    Reg Int {- Registers -}
  | I32 Int {- Constants -}
  | REnv String Type Int {- Identifiers in the closure environment -}
  deriving (Eq, Show)

data Label = 
     LabelT Int 
   | LabelS String
   deriving (Eq,Show)

type FunLabel = Int 

entryresult = (LabelS "entry", LabelS "result")

data Op = 
  {- O 1º dest é o registo que guarda o resultado. O 2º e 3º são valores -}
    Add_i32 Dest Dest Dest  
  | Sub_i32 Dest Dest Dest
  | Mul_i32 Dest Dest Dest
  | Div_i32 Dest Dest Dest

 {- O 1º dest é o resultado e o 2º é o valor -}
  | New_var Type Dest Dest

  {- O 1º dest é o registo que vai receber o valor do 2º dest -}
  | Set_var Type Dest Dest

  {- 1º dest é o resultado que guarda o conteúdo do registo do 2ºdest-}
  | Get_var Type Dest Dest
  | Free_var Dest

  {- o 1º dest recebe o resultado. Se a execução veio do 1º label, então o valor é o 2º dest. Se a execução veio do 2º label, então o valor é do 3º dest -}
  | Phi Type Dest Dest Label Dest Label


  | Eq_i32 Dest Dest Dest
  | Not_i1 Dest Dest
  

  | Call_type Type Dest Dest Dest
  
  {- ?-}
  | Get_env Type Dest Dest Dest String Int

  {- ?-}
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

emptyState :: ([Int],Int,Int) 
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

