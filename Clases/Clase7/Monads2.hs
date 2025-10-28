import Control.Monad.State

import Data.Hashable
-- stack install hashable 
import Data.Int
import Data.Char


-- 'e' és el tipus de l'estat
-- 'a' és el tipus del resultat
data Estat e a = Estat (e -> (a, e))


-- extreu la funció
execEstat :: Estat e a -> e -> (a, e)
execEstat (Estat g) = undefined

-- retorna la funcio que aplicada un estat
-- retona el valor resultat
execValor :: Estat e a -> e -> a
execValor (Estat g) = undefined


-- obtenir l'estat
obtenir :: Estat e e
obtenir = undefined


-- canviar l'estat
canviar :: e -> Estat e ()
canviar est = undefined


---------------------------------------------------------------------
-----------------------      Exemple 1       ------------------------
---------------------------------------------------------------------

-- insereix un enter a la pila
posar :: Integer -> Estat [Integer] ()
-- posar i =  undefined
posar i =  undefined

-- insereix un enter a la pila
posarBind :: Integer -> Estat [Integer] ()
posarBind i =  undefined

-- Treu el primer element de la pila
-- Precondicio: Llista no buida
treure :: Estat [Integer] Integer
treure =  undefined

-- Treu el primer element de la pila
-- Precondicio: pila no buida
treureBind :: Estat [Integer] Integer
treureBind =  undefined

-- Consulta el primer element de la pila (sense treure'l)
-- Precondicio: pila no buida
ultim :: Estat [Integer] Integer
ultim =  undefined

-- Consulta el primer element de la pila (sense treure'l)
-- Precondicio: pila no buida
ultimBind :: Estat [Integer] Integer
ultimBind =  undefined
-- Treu els 2 primers enters de la pila 
-- fa la suma i la torna a posar a la pila
sumar :: Estat [Integer] ()
sumar =  undefined

-- Treu els 2 primers enters de la pila 
-- fa la suma i la torna a posar a la pila
sumarBind :: Estat [Integer] ()
sumarBind =  undefined


-- Treu els 2 primers enters de la pila 
-- els multiplica i posa el resultat a la pila
mult :: Estat [Integer] ()
mult =  undefined

-- Treu els 2 primers enters de la pila 
-- els multiplica i posa el resultat a la pila
multBind :: Estat [Integer] ()
multBind =  undefined

-- Canvia el signe del primer element de la pila
oposat :: Estat [Integer] ()
oposat =  undefined

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
    return = undefined
    (Estat fx) >>= g = undefined
