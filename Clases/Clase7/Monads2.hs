module Clases.Clase7.Monads2 where

import Data.Hashable
import Data.Int
import Data.Char


-- 'e' és el tipus de l'estat
-- 'a' és el tipus del resultat
data Estat e a = Estat (e -> (a, e))


-- extreu la funció
execEstat :: Estat e a -> e -> (a, e)
execEstat (Estat g) = g

-- retorna la funcio que aplicada un estat
-- retona el valor resultat
execValor :: Estat e a -> e -> a
execValor (Estat g) = fst . g


-- obtenir l'estat
obtenir :: Estat e e
obtenir = Estat $ \est -> (est, est)


-- canviar l'estat
canviar :: e -> Estat e ()
canviar est = Estat $ \_ -> ((), est)


---------------------------------------------------------------------
-----------------------      Exemple 1       ------------------------
---------------------------------------------------------------------

-- insereix un enter a la pila
posar :: Integer -> Estat [Integer] ()
-- posar i =  Estat $ \est -> ((), i:est)
posar i = do
    pila <- obtenir
    canviar (i:pila)

-- insereix un enter a la pila
posarBind :: Integer -> Estat [Integer] ()
posarBind i = obtenir >>= (\pila -> canviar (i:pila) )

-- Treu el primer element de la pila
-- Precondicio: Llista no buida
treure :: Estat [Integer] Integer
treure = do
    pila <- obtenir
    canviar (tail pila)
    pure (head pila)

-- Treu el primer element de la pila
-- Precondicio: pila no buida
treureBind :: Estat [Integer] Integer
treureBind =  obtenir >>= (\pila -> canviar (tail pila) >> pure (head pila) )

-- Consulta el primer element de la pila (sense treure'l)
-- Precondicio: pila no buida
ultim :: Estat [Integer] Integer
-- ultim = do head <$> obtenir
ultim = do
    pila <- obtenir
    pure (head pila)

-- Consulta el primer element de la pila (sense treure'l)
-- Precondicio: pila no buida
ultimBind :: Estat [Integer] Integer
ultimBind =  obtenir >>= (\pila -> pure (head pila) )

-- Treu els 2 primers enters de la pila 
-- fa la suma i la torna a posar a la pila
sumar :: Estat [Integer] ()
sumar = do
    x <- treure
    y <- treure
    posar (x + y)

-- Treu els 2 primers enters de la pila 
-- fa la suma i la torna a posar a la pila
sumarBind :: Estat [Integer] ()
sumarBind = treure >>= (\x -> treure >>= (\y -> posar (x + y)) )


-- Treu els 2 primers enters de la pila 
-- els multiplica i posa el resultat a la pila
mult :: Estat [Integer] ()
mult = do
    x <- treure
    y <- treure
    posar (x * y)

-- Treu els 2 primers enters de la pila 
-- els multiplica i posa el resultat a la pila
multBind :: Estat [Integer] ()
multBind =  treure >>= (\x -> treure >>= (\y -> posar (x * y)) )

-- Canvia el signe del primer element de la pila
oposat :: Estat [Integer] ()
oposat = do
    x <- treure
    posar ((-1)*x)

-- Canvia el signe del primer element de la pila
oposatBind :: Estat [Integer] ()
oposatBind = treure >>= (\x -> posar ((-1)*x) )


-- posar un enter i obtenir la pila amb l'enter 
ex1 :: ([Integer], [Integer])
ex1 =  undefined

-- posar un enter i obtenir la pila amb l'enter 
ex1Bind =  undefined


-- canviar la pila perque nomes contingui els valors 1 i 2
-- despres afegir l'enter 3 i retornar l'enter 10.
ex2 :: (Integer, [Integer])
ex2 =  undefined

-- (a+b)*c
ex3 :: Estat [Integer] Integer
ex3 =  undefined


-- (2+3)*6
execEx3 :: Integer
execEx3 =  undefined


---------------------------------------------------------------------
-----------------------      Exemple 2       ------------------------
---------------------------------------------------------------------

-- A partir d'una llavor (de tipus String) que es l'estat inicial, 
-- produeix un enter pseudoaleatori (hash de la llavor) i canvia la 
-- llavor a una nova que és la concatenació de la llavor amb si 
-- mateixa

enterPsd :: Estat String Int
enterPsd =  undefined


tupla4EntersPsd :: Estat String (Int,Int,Int,Int)
tupla4EntersPsd = undefined

-- quin valor retorna?
tupla4ValorsPsd = execValor tupla4EntersPsd "b"


llistaEntersPsd :: Estat String [Int]
llistaEntersPsd =  undefined

exEntersPsd = execEstat llistaEntersPsd "abcd"



---------------------------------------------------------------------
----------------------      Instancies       ------------------------
---------------------------------------------------------------------

instance Functor (Estat e) where
    fmap f (Estat g) = undefined

instance Applicative (Estat e) where
    pure x = undefined
    (Estat fg) <*> (Estat fx) = undefined

instance Monad (Estat e) where
    (Estat fx) >>= g = undefined
