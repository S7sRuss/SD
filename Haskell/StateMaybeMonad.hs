module StateMaybeMonad where

{- ©2013 João Costa Seco, ICL DI-FCT-UNL -}

newtype StateMaybe s a = SM( s -> (s, Maybe a) )

instance Monad (StateMaybe s)
  where
    -- (>>=) :: StateMaybe s a -> (a -> StateMaybe s b) -> StateMaybe s b
    (SM p) >>= k  =  SM( \s0 -> case p s0 of 
                                  (s1, Just a) -> let (SM q) = k a in q s1 
                                  (s1, Nothing) -> (s1,Nothing))
     			   	
    -- return :: a -> StateTrans s a
    return a = SM( \s -> (s, Just a) )

raise_error :: StateMaybe s t 
raise_error = SM( \s -> (s,Nothing))

get :: StateMaybe s s
get = SM( \s -> (s,Just s) )

put :: s -> StateMaybe s s
put n = SM( \s -> (n, Just n))

inc :: StateMaybe Int Int
inc = SM( \s -> (s+1, Just (s+1)) )

newId :: StateMaybe Int String
newId = SM( \s -> (s+1, Just ("$" ++ (show s))))

