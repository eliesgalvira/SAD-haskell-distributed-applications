# Laboratori DAT / 4 

Índex
1 Classes i instàncies ..... 2
2 Les classes Semigroup i Monoid ..... 2
3 La classe Foldable ..... 3
3.1 Exercicis amb llistes ..... 4

# 1 Classes i instàncies 

Una classe defineix el tipificat d'un grup de funcions (opcionalment pot implementar-les), que seran comunes a tots els tipus de dades que instaciín la classe. La sintaxi és:

```
class (ClassePare t) => NomClasse t where
    funcio_1 :: tipificat_1
    ...
    funcio_n :: tipificat_n
    funcio_ :: tipificat_
    funcio_ = expressio_
```

on $t$ és una variable que representa un tipus o família de tipus. La part (ClassePare t) => és opcional i permet definir subclasses.

Exercici: Definir la classe Iterador, pensada per a tipus contenidors de valors, que inclou les funcions:

- ele::t a -> a, que retorna el primer element de t.
- seg::t a -> t a, que elimina el primer element de t.
- hasnext::t -> Bool, que retorna False en cas de t buit i True en cas contrari.

La sintaxi per a instanciar una classe és:

```
instance (ClasseAltra t) => ClasseAInstanciar t where
    funcio_1 = expressio_1
```

on t és el tipus per al que es vol definir la instància. La part (ClasseAltra t) => és opcional i obliga a que el tipus que s'està instanciant ja tingui definida una instància de ClasseAltra.

Exercici: Instanciar la classe Iterador per al tipus Llista de la pràctica anterior.
Exercici: Usant l'iterador, definir una funció sumElem: : (Iterador t) => t Int -> Int, que suma els elements d'un contenidor d'enters.

Haskell permet instanciar automàticament certes classes. Es fa utilitzant la primitiva deriving en la definició del tipus. Per exemple

```
data Llista a = Buida | L a (Llista a)
    deriving Show
```

instancia la classe Show per al tipus Llista, i per tant es podran representar els valors com a String
Exercici: Instanciar les classes Eq, Ord i Enum per al tipus Nat de la pràctica anterior.

## 2 Les classes Semigroup i Monoid

En matemàtiques un semigrup és un conjunt amb una operació associativa. Per exemple els naturals amb la suma són un semigrup. Haskell defineix la classe Semigroup que captura aquesta propietat.

```
class Semigroup m where
    (<>) :: m -> m -> m
```

Un fet important, que apareixerà en altres classes, és que les funcions de la classe han de satisfer certes propietats (normalment matemàtiques), i que és responsabilitat de les instàncies complir-les. En el cas de Semigroup es demana l'associativitat de l'operació, és a dir
$(\mathrm{x}<>\mathrm{y})<>\mathrm{z}=\mathrm{x}<>(y<>\mathrm{z})$

Exercici: Per al tipus [a], quines de les següents instàncies són correctes:

1) $x<y=y+x$
2) $x<y=x+y$
3) $x<y=x: y$

Exercici: Definir la instància de Semigroup per al tipus Nat.
Un monoid és un conjunt amb una operació associativa i un element neutre per a aquesta operació, és a dir, un semigrup amb element neutre. Per exemple els números naturals amb la suma i el 0 com a element neutre són un monoid. Haskell defineix la classe Monoid com

```
class Semigroup m => Monoid m where
    mempty :: m
    -- defining mapped is unnecessary, it copies from Semigroup
    mapped :: m -> m -> m
    mapped = (<>)
    -- defining mconcat is optional, since it has the following default:
    mconcat :: [m] -> m
    mconcat = ...
```

Les propietats perquè mempty actui com a element neutre són

```
-- Identity laws
x <> mempty = x
mempty <> x = x
```

Exercici: Definir la instància de Monoid per al tipus Nat. Veure que es compleixen les lleis d'identitat.

# 3 La classe Foldable 

Per a tipus contenidors de valors, per exemple les llistes, resulta útil poder combinar tots els valors per a obtenir un únic valor resultant (ex: sumar els elements d'una llista, ...). Haskell ofereix la classe Foldable per aquest propòsit:

```
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    -- All of the following have default implementations:
    fold :: Monoid m => t m -> m -- generalised mconcat
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a
    toList :: t a -> [a]
    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool
    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a
```

La funció foldMap té com a paràmetres una funció a -> m que aplica als elements de l'estructura (segon paràmetre) i els combina en un únic element usant l'operació $<>$ del monoid.

Les funcions foldr i foldl són semblants a foldMap, però en aquest cas la funció que es passa com a paràmetre és la que combina el valor acumulat amb els valors de l'estructura (per això és una funció de dos paràmetres). Notar que també es passa com a paràmetre un valor inicial. Per a entendre aquestes funcions és útil estudiar com estan definides per a llistes:

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

Exercici: Definir la instància de Foldable per al tipus Llista.
Exercici: Per què no es pot definir una instància de Foldable per al tipus Nat?. Definir la funció foldNat : : (a -> a) -> a -> Nat -> a. Utilitzar-la per a redefinir les funcions natAint i sumNat.

# 3.1 Exercicis amb llistes 

Resoldre els següents exercicis sabent que el tipus llista és una instància de la classe Foldable.
Exercici: Calcular el màxim d'una llista d'enters: maxInt: : [Int] -> Int.
Exercici: Definir una funció que donada una llista d'elements de tipus Maybe retorni el número de Nothings.
Exercici: Definir una funció que donada una llista d'enters retorni una tupla amb l'element màxim i l'element mínim.
Exercici: Definir una funció que donada una llista d'enters en retorni la mitjana aritmètica. Pista: Definir primer una funció que donada una llista d'enters retorni una tupla amb el número d'elements i la seva suma.
Exercici: Definir una funció totsMax que donada una llista d'enters retorna una llista amb totes les aparicions del valor màxim. Per exemple, totsMax $[2,4,1,4,3,2,1,4]=>[4,4,4]$.
Exercici: Definir una funció posicionsLletra que donat un caràcter i un String retorna una llista amb les posicions on apareix el caràcter. Per exemple, posicionsLletra 'a' "patata" => [1,3,5].
Exercici: Donada una llista de llistes d'enters, [[Int]], definir les funcions següents:

- sumPar: : [[Int]] -> [Int], que retorna una llista amb la suma de cada subllista.
- sumTot: : [[Int]] -> Int, que suma tots els elements de totes les llistes.

Exercici: Definir la funció separar: :Ord a => [a] -> a -> ([a], [a]), que donada una llista i un valor, retorna una tupla de dos llistes on la primera conté els valors menors o iguals que el valor i la segona la resta de valors.
Exercici: Usar la funció anterior per a ordenar una llista: ordenar: :Ord a => [a] -> [a].



