module Syntax where

import qualified Data.List as List


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
