data M3 = A | B | C


cert :: a -> b -> a
cert x _ = x

fals :: a -> b -> b
fals _ y = y

(.<.) :: M3 -> M3 -> (a -> a -> a)
A .<. B = cert
A .<. C = cert
B .<. C = cert
_ .<. _ = fals
