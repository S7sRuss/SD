module Generator where

import qualified Data.List as List
import Text.StringTemplate 

import Syntax
import Env
import CoreCompiler

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

type2LLVM UnitType = "void"

type2LLVM t = error "type2LLVM "++ show t

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


format IntType = "%d"

format (FunType _ _) = "%p"

format (RefType _) = "%p"

format BoolType = "%d"

format UnitType = ".."

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
        (if t /= UnitType then 
           "  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* "++
           "@.str, i64 0, i64 0), "++ type2LLVM t++" "++ (dest2Str dest) ++
           ") nounwind\n"
        else "")++
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
       "declare void @free_var(%struct.var_type*)\n"++
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

