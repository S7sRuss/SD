module TypeSystem where

import Env
import Syntax
import MaybeMessageMonad

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

typeOf (Free e) = UnitType

typeOf (If _ _ _ t ) = t

typeOf (While _ _) = UnitType

typeOf Skip = UnitType

typeOf (Seq _ e) = typeOf e

typeOf (Fun _ _ _ t) = t

typeOf (Call _ _ t) = t


{- -}

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

typecheck (Free e) env = 
          do
          e_t <- typecheck e env
          case typeOf e_t of
               RefType t -> return (Free e_t)
               _ -> Message "Not a reference type in free expression."

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