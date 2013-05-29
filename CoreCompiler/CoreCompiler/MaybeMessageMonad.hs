module MaybeMessageMonad where

data MaybeMessage a = 
       Result a 
     | Message String
     deriving (Eq, Show) 

instance Monad MaybeMessage
  where
    -- (>>=) :: MaybeMessage a -> (a -> MaybeMessage b) -> MaybeMessage b
    (Result v) >>= k  =  k v
    (Message s) >>= k =  Message s
     			   	
    -- return :: a -> MaybeMessage a
    return a = Result a

raise_message :: String -> MaybeMessage a 
raise_message s = Message s
