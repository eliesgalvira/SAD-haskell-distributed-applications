# Laboratori DAT / 3 

Índex
1 Tipus ..... 2
1.1 Expressions de tipus ..... 2
1.2 Constructors de valors paramètrics ..... 2
1.3 Patrons ..... 2
1.4 Definicions recursives ..... 3
1.5 Constructors de tipus paramètrics ..... 3
1.6 Llistes ..... 3

# 1 Tipus 

Un tipus defineix un conjunt de valors. La definició més bàsica és

$$
\text { data } T=V
$$

on data indica que es tracta d'una definició de tipus, $T$ s'anomena constructor de tipus, $V$ constructor de valors i són funcions que retornen un conjunt de valors.

En la definició (1) les dues funcions són constants (sense paràmetres). Un constructor constant $V$ retorna com a valor $\{V\}$ (com a literal), per tant els valors del tipus $T$ són $T=\{V\}$.

### 1.1 Expressions de tipus

Per a operar amb diferents constructors es disposa de l'operació | que semànticament és la unió (a vegades s'anomena suma). Per exemple

$$
\text { data } \mathrm{T}_{\mathrm{n}}=\mathrm{V}_{1}|\ldots| \mathrm{V}_{\mathrm{n}}
$$

defineix el conjunt de valors $\mathrm{T}_{\mathrm{n}}=\left\{\mathrm{V}_{1}\right\} \cup \cdots \cup\left\{\mathrm{V}_{\mathrm{n}}\right\}=\left\{\mathrm{V}_{1}, \ldots, \mathrm{~V}_{\mathrm{n}}\right\}$. Un exemple és el tipus booleà predefinit com a Bool = False I True, i amb valors Bool = \{False, True\}.
Exercici: Definir el tipus Bit que permet representar un bit, amb valors Bit $=\{0, I\}$.
Exercici: Definir la funció (.==.): :Bit -> Bit -> Bool que retorna True si i només si els dos bits són iguals.

### 1.2 Constructors de valors paramètrics

Els constructors de valors, com a funcions que són, admeten paràmetres. Els paràmetres obligatóriament són conjunts de valors, representats per funcions constructores de tipus.

En cap moment es defineix l'expressió de la funció constructora de valors, però no cal, ja que el valor de retorn és el producte cartesià dels conjunts de valors dels paràmetres. Per exemple

$$
\text { data } F=F_{1} T_{n}
$$

on $\mathrm{T}_{\mathrm{n}}$ s'ha definit a (2). Avaluar $\mathrm{F}_{1} \mathrm{~T}_{\mathrm{n}}=\mathrm{F}_{1}\left\{\mathrm{~V}_{1}, \ldots, \mathrm{~V}_{\mathrm{n}}\right\}=\left\{\mathrm{F}_{1} \mathrm{~V}_{1}, \ldots, \mathrm{~F}_{1} \mathrm{~V}_{\mathrm{n}}\right\}$, i per tant els valors $\mathrm{d}^{\prime} \mathrm{F}$ són $F=\left\{F_{1} V_{1}, \ldots, F_{1} V_{n}\right\}$.
Exercici: Definir el tipus DBit per a representar números d'exactament 2 Bits: data DBit = D ....
Exercici: Definir la funció
dbitAint : : DBit -> Int
dbitAint (D x y) =
que retorna el valor enter del número binari.

### 1.3 Patrons

A l'apartat 1.2 es representen els valors de $F$ com $F=\left\{F_{1} V_{1} \| i=1 . . n\right\}$. Una alternativa és $F=$ $\left\{F_{1} \mathrm{x} \| \mathrm{x} \in \mathrm{T}_{\mathrm{n}}\right\}$, i es diu que $\mathrm{F}_{1} \mathrm{x}$ és un patró per als valors de F. Llavors, per definició, tots els valors del llenguatge encaixen en algun patró.

Haskell admet l'ús de patrons i a més ofereix una funcionalitat de deconstrucció: quan s'avalua l'encaix d'un valor amb un patró, les variables del patró s'instancien als valors corresponents i es poden usar en el codi. Un exemple és la definició de dbitAint (D x y) de l'exercici anterior. Un altre exemple:

$$
\text { data } G=\text { G1 Int Int }
$$

```
divG(G1 x y) = if y /= 0 then div x y
    else undefined
```

Exercici: Utilitzant només patrons (sense guardes ni if ..), implementar la funcio dbitAintP: :DBit -> Int que retorna el valor enter representat pel número binari.

# 1.4 Definicions recursives 

També es poden fer definicions recursives quan el tipus del paràmetre és el propi tipus que s'està definint. Per exemple

$$
\text { data Nat }=\mathrm{S} \text { Nat }
$$

és una definició recursiva (acceptada per Haskell), però que no pot produir cap valor, ja que la recursivitat no acaba. En tota definició recursiva hi ha d'haver una condició de parada. Aquí passa per definir un altre constructor de valors no recursiu. Una solució és

$$
\text { data Nat }=\text { Zero | } \mathrm{S} \text { Nat }
$$

on Zero és el constructor no recursiu. Llavors el conjunt (infinit) de valors és

$$
\text { Nat } \rightarrow\{\text { Zero, } S \text { Zero, } S(S \text { Zero }), S(S(S \text { Zero })), \ldots\} .
$$

Exercici: La definició de tipus anterior es pot utilitzar per a representar números naturals. Implementar una funció natAint que donat un valor de tipus Nat retorni el corresponent Int.
ghci> natAint (S(S(S Zero)))
3
Exercici: Implementar la suma de dos valors de tipus Nat: sumaNat::Nat -> Nat -> Nat.
ghci> natAint \$ sumaNat (S(S(S Zero))) (S(S Zero))
6
Exercici: Definir un tipus de dades MInt que permeti representar enters (ara també negatius).
Exercici: Definir les funcions mintAint::MInt -> Int i intAmint::Int -> MInt que facin la conversió entre tipus.

Exercici: Definir la suma: sumaMInt: : MInt -> MInt -> MInt. Es poden utilitzar les funcions de conversió.

Exercici: Definit la suma: sumaMIntD: : MInt -> MInt -> MInt. No es poden utilitzar les funcions de conversió.

### 1.5 Constructors de tipus paramétrics

La sintaxi és l'habitual de les funcions, amb els paràmetres separats per espais. El cas més senzill, amb un únic paràmetre, és

$$
\text { data } T P x=P x
$$

on x és el paràmetre, que com en els constructors de valors, obligatòriament ha de ser un altre constructor de tipus.

En una funció paramètica el valor de retorn depèn del paràmetre, llavors el que s'està definint a (4) és una funció que permet crear una família de tipus. Per a cada valor concret del paràmetre es crea un tipus diferent.

### 1.6 Llistes

El tipus llista, un tipus paramètric i recursiu, és un dels tipus més utilitzats. La definició podria ser

```
data Llista a = B | L a (Llista a)
    deriving Show -- Permet mostrar Llistas per pantalla
```

Haskell ja ofereix un tipus llista predefinit però amb una sintaxi especial. La definició del tipus llista predefinit és equivalent a
data [] a = [] | (:) a ([] a)
on el constructor (:) es pot utilitzar de forma infixa. El tipus [] a també es pot expressar com [a].
El tipus llista predefinit de Haskell i el tipus Llista definit anteriorment són isomòrfics. Això vol dir que existeien dues funcions de conversió

desdeL : : Llista a -> [a]
aL : : [a] -> Llista a
que cumpleixen les propietats

$$
\begin{aligned}
& \text { (aL . desdeL) == (id : : Llista a -> Llista a) } \\
& \text { (desdeL . aL) == (id : : [a] -> [a]) }
\end{aligned}
$$

Exercici: Defineix les funcions de conversió desdeL i aL:
ghci> desdeL \$ L 1 (L 2 (L 3 B))
$[1,2,3]$
ghci> aL $[1,2,3]$
L 1 (L 2 (L 3 B))
Exercici: Defineix les següents funcions:

1. Crea una llista amb els n enters (de n a 1):
initL : : Int -> Llista Int
ghci> initL 5
L 5 (L 4 (L 3 (L 2 (L 1 B))))
ghci> desdeL \$ initL 5
$[5,4,3,2,1]$
2. Gira el contingut d'una llista:
giraL : : Llista a -> Llista a
ghci> desdeL \$ giraL \$ aL [1,2,3]
$[3,2,1]$
ghci> desdeL \$ giraL \$ initL 5
$[1,2,3,4,5]$
3. Crea una llista de llistes a partir d'un enter (veure exemples) i defineix també la funció desdeLdL per convertir a llistes de Haskell:
initLdL : : Int -> Llista (Llista Int)
desdeLdL : : Llista (Llista a) -> [[a]]
ghci> initLdL 3
L (L 3 (L 2 (L 1 B))) (L (L 2 (L 1 B)) (L (L 1 B) B))
ghci> desdeLdL \$ initLdL 3
$[[3,2,1],[2,1],[1]]$
4. Aplasta una llista de llistes en una llista d'elements:
aplastaL : : Llista (Llista a) -> Llista a
ghci> aplastaL (L B (L (L 1 B) B))
L 1 B
ghci> aplastaL \$ initLdL 3
L 3 (L 2 (L 1 (L 2 (L 1 (L 1 B)))))
ghci> desdeL \$ aplastaL \$ initLdL 3
$[3,2,1,2,1,1]$
Les funcions initLdL i desdeLdL anteriors, que s'han definit de forma recursiva, les podem redefinir a partir d'una funció de mapeig i sense usar recursivitat.

Exercici: Defineix la funció de mapeig mapejaL:
mapejaL : : (a -> b) -> Llista a -> Llista b
ghci> mapejaL even (L 1 (L 2 (L 3 B)))
L False (L True (L False B))
Exercici: Defineix la funció initLdL2 no recursiva, equivalent a initLdL, usant mapejaL i initL.
Exercici: Defineix la funció desdeLdL2 no recursiva, equivalent a desdeLdL, usant mapejaL i desdeL.



