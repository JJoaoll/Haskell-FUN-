module Nat where 

data Nat = O | S Nat deriving(Eq)

plus :: Nat -> Nat -> Nat 
plus n O     = n 
plus n (S m) = S (plus n m)

--(+) :: Nat -> Nat -> Nat 
--(+) = plus

instance (Show Nat) where
  show O     = "O"
  show (S n) = 'S' : show n
 -- show (S n) = "S" ++ show n 
 
  -------------gambiarras:--------------
{-instance (Show Nat) where
   show n = show (natToInt n)

--(+) :: Nat -> Nat -> Nat 
--(+) = plus

intToNat :: Integer -> Nat 
intToNat x = if x <= 0 
  then O 
  else S (intToNat (x - 1)) 

natToInt :: Nat -> Integer 
natToInt O = 0 
natToInt (S n') = 1 + natToInt n' -}
