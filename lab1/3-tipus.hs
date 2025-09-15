data M3 = A | B | C
  deriving(Show)


cert :: a -> b -> a
cert x _ = x

fals :: a -> b -> b
fals _ y = y

(.<.) :: M3 -> M3 -> (a -> a -> a)
A .<. B = cert
A .<. C = cert
B .<. C = cert
_ .<. _ = fals


max2 :: M3 -> M3 -> M3
max2 x y = (x .<. y) y x

maxim :: M3 -> M3 -> M3 -> M3
maxim a b c = max2 (max2 a b) c

maximDup :: M3 -> M3 -> M3 -> M3
maximDup a b c = ((max2 a b) .<. c) c (max2 a b)

maximLet :: M3 -> M3 -> M3 -> M3
maximLet a b c =
  let ab = max2 a b
  in (ab .<. c) c ab

maximWhere :: M3 -> M3 -> M3 -> M3
maximWhere a b c = (ab .<. c) c ab
  where
    ab = max2 a b
