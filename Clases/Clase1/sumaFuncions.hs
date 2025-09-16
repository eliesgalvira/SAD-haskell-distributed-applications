sumaFuncions :: Num b => (a -> b) -> (a -> b) -> (a -> b)
sumaFuncions f g = h
    where h x = f x + g x

sumaFuncionsLambda :: Num b => (a -> b) -> (a -> b) -> (a -> b)
sumaFuncionsLambda f g = \x -> f x + g x
