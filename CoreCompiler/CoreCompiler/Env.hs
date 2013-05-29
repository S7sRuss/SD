module Env where

type Env a = [(String,a)]

find :: Eq a => String -> Env a -> Maybe a 

find x env = if l == [] then Nothing else Just (head l)
             where l = [ v | (k,v) <- env, k == x]

assoc :: Env a -> String -> a -> Env a

assoc env x l = ((x,l):env)

assocLast :: Env a -> String -> a -> Env a

assocLast env x l = env ++ [(x,l)]