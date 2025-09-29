# Laboratori DAT / 2 

Índex
1 Tipificat de números ..... 2
2 Composició de funcions ..... 2
3 Recursivitat ..... 3

# 1 Tipificat de números 

En la compilació s'assigna un tipus a cada valor i funció del codi (que pot ser polimòrfic). Amb els números també passa i tenen una notació polimòrfica, és a dir, 2 tant pot representar un Int com un Double, etc

Exercici: Crear un nou fitxer, 02lab.hs i definir els següents valors (sense tipificar):
$\mathrm{p}=2$
$\mathrm{q}=1$
carregar el fitxer amb stack ghci 02lab.hs i amb la comanda ghci> :t consultar el tipus de cada valor inferit pel compilador (escriure'l com a comentari al costat de cada valor).

Afegir al fitxer la línia dr $=\mathrm{p} / \mathrm{q}$. Quins tipus infereix ara el compilador? (afegir-los al comentari previ). Afegint la línia de $=\operatorname{div} \mathrm{p}$ q no compila ja que el compilador és incapaç d'inferir els tipus. Esbrinar els tipus de les funcions / i de div (escriure'ls com a comentaris en el fitxer).

Finalment, usant una tipificació més genèrica i/o les funcions convertidores entre tipus numèrics ( https://wiki.haskell.org/Converting_numbers) aconseguir que compili.

## 2 Composició de funcions

L'associativitat a l'aplicar funcions és per l'esquerra: f g h x s'interpreta com (((f g) h) x. Llavors per a composar les funcions són necessaris els parèntesis $f(g(h x))$. Per evitar aquests parèntesis existeixen les funcions composició (.) i aplicació (\$) on f . g \$ x és equivalent a $f(g(x))$.
Exercici: Per a entendre millor les funcions composició i aplicació es demana redefinir-les. Usant ghci> : i (.) es poden obtenir les tipificacions de les funcions. Sense usar les funcions ja existents, definir les funcions:
$($ \$.) : : (a -> b) -> a -> b -- aplicació
(...) : : (b -> c) -> (a -> b) -> (a -> c) -- composició

Exercici: Utilitzant les definicions anteriors de composició i aplicació definir la funció

```
arrodonir :: Double -> Integer -> Double
arrodonir valor numDec =
```

que arrodoneix valor a numDec decimals (pot ser útil la funció round).
Exercici: Repetir l'exercici anterior (definir arrodonirb) usant les funcions de composició i aplicació ja predefinides. Per què en aquest cas els parèntesis necessaris són diferents?

## Exercici:

1. Definir la funció elevar al quadrat: quad : : Int -> Int
2. Definir la funció : multiplicar : : Int -> Int -> Int
3. Definir la funció: triple : : Int -> Int
4. Fent servir les funcions anteriors i la composició, definir la funció triplequad que calcula el triple d'un enter i eleva el resultat al quadrat: triplequad : : Int -> Int
5. Fent servir les funcions anteriors i la composició, definir la funció multiplicarquad que calcula la multipicació de dos enters i eleva el resultat al quadrat: multiplicarquad : : Int -> Int -> Int

Exercici: Reescriure, usant composició, amb el mínim número de parèntesis:
f1 $x=$ inc (inc (inc $x)+($ inc $x)$ )
f2 $x=(+)$ (inc (inc $x))$ (inc $(x+x))$
on
inc $x=x+1$
Exercici: (seqüenciació) Hi ha situacions on és útil poder escriure la composició de forma seqüencial (en ordre invers), és a dir, $f(g(h(x)))=>h x .>g .>f$. Definir la funció (. >), per exemple res $=$ inc $0 .>$ inc . $>$ inc dóna 3 .

# 3 Recursivitat 

Una definició recursiva és aquella on el terme definit apareix en la definició: $f=\ldots f \ldots$ Haskell admet definicions recursives, s'ha de tenir en compte però (el compilador no ho controla) que la recursivitat acaba. La sintaxi és simplement invocar la funció que s'està definint en l'expressió que la defineix. La recursivitat és l'única manera de definir iteracions/bucles en Haskell.
Exercici: Fent servir recursivitat calcular la divisió entera i la resta:

```
divisio :: Int -> Int -> Int
residu :: Int -> Int -> Int
```

Exercici: (Coeficient binomial) Sabent que $\binom{n}{k}=\binom{n-1}{k}+\binom{n-1}{k-1}$ definir una funció que calculi $\binom{n}{k}$, on $\binom{n}{0}=1$ i $\binom{0}{k}=0$
Exercici: Definir dues funcions, sumaN que retorni el sumatori dels $N$ primers números i sumaNPar que retorni el sumatori dels $N$ primers números parells.
Exercici: (Generalització del sumatori) Les dues funcions de l'exercici anterior són iguals i només es diferencien pels valors que sumen. Llavors es pot generalitzar definint una funció

$$
\operatorname{sumaG}=\sum_{i=1}^{N} f(i)
$$

on $f$ és una funció qualsevol (sumable).
Es demana definir sumaG i definir les funcions de l'exercici anterior, que ara s'anomenaran sumaNG i sumaNParG utilitzant sumaG
Exercici: Implementar el següent sumatori

$$
\text { sumatoriD }=\sum_{i=1}^{n} \sum_{j=1}^{n} f(i) g(j)
$$

on
ghci> sumatoriD id id 3
36

Exercici: (derivada) La derivada d'una funció $f$ en un punt $p$ es pot definir com

$$
d f(p)=\lim _{h \rightarrow 0} \frac{f(p+h)-f(p)}{h}
$$

Es demana implementar la funció drvd : : (Double -> Double) -> Double -> Double que calcula la derivada d'una funció en un punt. Per a fer-ho s'ha de calcular la derivada per a valors cada cop més petits d'h, i acabar les iteracions un cop dos resultats consecutius difereixen en menys de $10^{-6}$. Per exemple:
res01 = drvd ( 2) (-2)
ghci> res01
$-3.9999999002$