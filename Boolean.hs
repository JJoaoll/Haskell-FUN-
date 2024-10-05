data Boolean = T | F deriving(Show, Eq) 
lnot :: Boolean -> Boolean 
lnot T = F
lnot _ = T 

land :: Boolean -> Boolean -> Boolean 
land T T = T
land _ _ = F

lor :: Boolean -> Boolean -> Boolean 
lor T _ = T 
lor _ x = x 

ifthenelse :: Boolean -> a -> a -> a 
ifthenelse T x _ = x 
ifthenelse F _ y = y
