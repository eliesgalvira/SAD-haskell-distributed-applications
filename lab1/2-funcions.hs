primer :: a -> b -> a
primer x _ = x

segon :: a -> b -> b
segon _ y = y

primerL :: a -> b -> a
primerL = \x -> \_ -> x

segonL :: a -> b -> b 
segonL = \_ -> \y -> y

r1 = primer (segon 1 2) 3
r2 = segon (primer 1 2) 3
r3 = primer (1 + segon 1 2 + 1) 2
r4 = primer segon 1 2 3

