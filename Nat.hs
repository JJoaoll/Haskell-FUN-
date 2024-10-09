module Nat where 

data Nat = O | S Nat deriving(Eq)

plus :: Nat -> Nat -> Nat 
plus n O     = n 
plus n (S m) = S (plus n m)

mult :: Nat -> Nat -> Nat 
mult O m     = O 
mult (S n) m = m  `plus` mult n m 


potn :: Nat -> Nat -> Nat 
potn  n O     = S O 
potn  n (S m) = n `mult` (n `potn` m)

double :: Nat -> Nat
double n = n `plus` n

n = S $ S $ S O 
m = n `plus` S (S O) 
--(+) :: Nat -> Nat -> Nat 
--(+) = plus

fact :: Nat -> Nat 
fact O     = S O 
fact (S n) = n `mult` fact n

fib :: Nat -> Nat 
fib O         = O 
fib (S O)     = S O 
fib (S (S n)) = fib n `plus` fib (S n) 



instance (Show Nat) where
  show O     = "O"
  show (S n) = 'S' : show n
 -- show (S n) = "S" ++ show n 
 
  -------------gambiarras:--------------
{-instance (Show Nat) where
   show n = show (natToInt n)

--(+) :: Nat -> Nat -> Nat 
--(+) = plus
-} 
intToNat :: Integer -> Nat 
intToNat x = if x <= 0 
  then O 
  else S (intToNat (x - 1)) 

natToInt :: Nat -> Integer 
natToInt O = 0 
natToInt (S n') = 1 + natToInt n' 
