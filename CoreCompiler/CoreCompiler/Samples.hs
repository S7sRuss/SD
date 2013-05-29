module Samples where

import Syntax 

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

f = Decl "f" (Fun "x" IntType (Add (Id "x" None) (Add (Id "x" None) (Num 1))) None) (Call (Id "f" None) (Num 1) None) None

f' = Decl "y" (Num 0) (Decl "f" (Fun "x" IntType (Add (Id "x" None) (Add (Id "x" None) (Id "y" None))) None) (Call (Id "f" None) (Num 1) None) None) None

g = Decl "g" (Fun "x" IntType (Add (Id "x" None) (Num 1)) None) (Call (Id "g" None) (Num 1) None) None

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

