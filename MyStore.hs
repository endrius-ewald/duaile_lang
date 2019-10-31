-------------------------------------------------------------------------
--  
--     Store.hs
--  
--         An abstract data type of stores of integers, implemented as
--         a list of pairs of variables and values.         
--                                  
--         (c) Addison-Wesley, 1996-2011.                   
--  
-------------------------------------------------------------------------

module MyStore 
   ( Store, 
     initial,     -- Store
     value,       -- Store -> Var -> Integer
     update       -- Store -> Var -> Integer -> Store
    ) where

-- Var is the type of variables.                    

type Var = String

-- The implementation is given by a newtype declaration, with one
-- constructor, taking an argument of type [ (Integer,Var) ].

data Store = Store [ (Integer,Var) ] 

instance Eq Store where 
  (Store sto1) == (Store sto2) = (sto1 == sto2)                 

instance Show Store where
  showsPrec n (Store sto) = showsPrec n sto                 
--  
initial :: Store 

initial = Store []

value  :: Store -> Var -> Integer

value (Store []) v         = 0
value (Store ((n,w):sto)) v 
  | v==w            = n
  | otherwise       = value (Store sto) v

update  :: Store -> Var -> Integer -> Store
--TODO: Melhorar o update para sobreescrever a var
update (Store sto) v n = Store ((n,v):sto)

