# Laboratori DAT / 1

Índex
1 Entorn de treball ..... 2
2 Funcions ..... 2
2.1 Notació lambda ..... 2
2.2 Tipificat ..... 3
2.3 Prioritat/Associativitat ..... 3
2.4 Notació infixa ..... 3
2.5 Funcions d'ordre superior ..... 3
2.6 Aplicació parcial ..... 4
3 Tipus ..... 4

# 1 Entorn de treball

Per a compilar i executar codi Haskell una opció és instal$\cdot$lar stack, disponible a la web haskellstack.org.
Per a començar es veu com crear una funció i executar-la mitjançant el ghci. El ghci és un entorn interactiu en el qual s'hi poden avaluar expressions en Haskell i també interpretar programes. Seguir els següents passos:

1. Crear un fitxer amb extensió .hs, per exemple inici.hs.
2. Escriure el següent codi en el fitxer:
```
inc :: Double -> Double
inc i = i + 1
```

3. Obrir un terminal i situar-se en el directori on s'ha creat el fitxer.
4. Executar la comanda stack ghci inici.hs.
5. El prompt ha de canviar a ghci>
6. En aquest prompt escriure inc 4 i prémer return.

Haskell és un llenguatge molt intuïtiu, la declaració
inc :: Double -> Double
indica que inc és una funció que admet un paràmetre de tipus nombre real i retorna un valor també de tipus real. El que es fa a inc 4 és avaluar la funció inc en el valor 4.

El codi dels següents exercicis es pot anar afegint al fitxer inici.hs. Després de cada canvi i per a poder avaluar-los, s'ha de re-carregar el fitxer fent ghci>:reload o també ghci>:r.

## 2 Funcions

La sintaxi de definició de funcions és

$$
\text { nomFuncio } \mathrm{p}_{1} \mathrm{p}_{2} \ldots \mathrm{p}_{\mathrm{n}}=\text { expressio }
$$

on $\mathrm{p}_{1}$ representen els arguments, separats per espai. Per a invocar la funció s'escriu el nom de la funció i els valors dels arguments separats per espais. L'avaluació de la funció consisteix en substituir els valors dels paràmetres en l'expressio i reduir/simplificar al màxim l'expressió per a obtenir el valor de retorn (que pot ser una funció).
Exercici: Definir la funció primer que rep dos arguments i retorna el primer. Definir la funció segon que rep dos arguments i retorna el segon.

### 2.1 Notació lambda

La notació lambda es basa en el càlcul lambda i té la forma:

$$
f=\backslash x \rightarrow \text { expressio }
$$

on es defineix la funció $f$, amb un argument $x$ i una expressio per a calcular el valor de retorn. Per a invocar la funció s'escriu el nom de la funció seguit del valor de l'argument (separat per un espai), per exemple: f 3.

La definició (1) inclou les funcions de més d'un paràmetre, doncs l'expressio pot ser una altra funció

$$
f=\backslash x \rightarrow(\underbrace{\mathrm{y} \rightarrow \exp}_{\text {expressio }})
$$

Els parèntesis fan explícita la prioritat, però l'operador $\rightarrow$ és associatiu per la dreta, per tant no caldrien.
Exercici: Definir les funcions primerL i segonL corresponents a les funcions primer i segon, però usant notació lambda.

# 2.2 Tipificat

En un algorisme tot valor ha de tenir assignat un tipus (o conjunt de tipus / polimorfisme). Per a fer l'assignació es disposa de la notació ::, que es pot llegir com és del tipus ... . Fer-ho és opcional, però en la compilació s'infereixen els tipus (és a dir, es tipifica) i es generen errors de tipus.

La sintaxi per a tipificar una funció és

$$
\text { nomFuncio }:: \mathrm{t} 1 \rightarrow \mathrm{t} 2 \rightarrow \ldots \rightarrow \mathrm{tn} \rightarrow \mathrm{tr}
$$

on ti representen els tipus dels arguments i tr el tipus del valor de retorn.
Exercici: Tipificar totes les funcions que s'han definit fins ara.

### 2.3 Prioritat/Associativitat

Per a avaluar expressions s'ha de conèixer l'associativitat i la prioritat dels operadors/funcions implicats. Pel que fa a les funcions, són associatives per l'esquerra i tenen prioritat màxima.
Exercici: Parentitzar les expressions de manera que el valor de retorn sigui l'indicat (és probable que s'hagin de modificar les tipificacions de primer i/o segon definides en l'exercici anterior):

- r1 = primer segon 123 -> r1 = 2
- r2 = segon primer 123 -> r2 = 3
- r3 = primer 1 + segon $12+12$->r3 = 4
- r4 = primer segon 123 -> r4 = 3


### 2.4 Notació infixa

Les funcions de dos arguments es poden usar en notació infixa (com a operadors), posant el nom de la funció entre accents oberts: 10 "primer" 5.

Si el nom de la funció està format només per caràcters especials $\{=><+--\ldots\}$ s'interpreta que és un operador i que es pot usar directament en forma infixa, sense les cometes, però s'haurà de posar entre parèntesis per a la notació funcional.
Exercici: Definir les funcions (?) i (??) equivalents a primer i segon respectivament. Reescriure les expressions de l'exercici anterior en notació infixa (anomenar-les r1i ...).

### 2.5 Funcions d'ordre superior

Les funcions accepten funcions com a paràmetres i poden retornar funcions com a valors (de fet en l'expressió r4 ja s'ha utilitzat aquesta propietat).
Exercici: Definir la funció inter que intercanvia l'ordre dels paràmetres d'una altra funció de dos paràmetres. Per exemple inter primer 12 retorna 2.
Exercici: Definir a partir de la funció primer la funció segonI equivalent a la funció segon.
Exercici: Parentitzar les següents expressions de manera que sempre retornin 0.

- e1 = (+) inter (-) 12 -1
- e2 = primer primer primer 0000
- e3 = 0 + primer segon 101
- e4 = div e1 e3, es poden reparentitzar e1 i/o e3

# 2.6 Aplicació parcial

Les funcions amb més d'un argument es poden veure com a funcions que retornen funcions. Un exemple és el següent:

```
(+.) :: Int -> Int -> Int
(+.) x y = x - y
```

és una funció que donats dos enters x i y retorna la seva diferència. Alhora,
$(+.)$ : : Int -> (Int -> Int)
$(+.) \mathrm{x} \mathrm{y}=\mathrm{x}-\mathrm{y}$
és una funció que donat un enter x retorna una funció que donat un enter y retorna la diferència entre x i y. Això permet definir la funció restarDe20 com:

```
restarDe20 :: Int -> Int
restarDe20 y = (+.) 20 y
```

El símbol -> és associatiu per la dreta:

$$
f:: a \rightarrow a \rightarrow a \rightarrow a
$$

equival a

$$
f:: a \rightarrow(a \rightarrow(a \rightarrow a))
$$

Aixó vol dir que:

$$
f \times y \times
$$

equival a

$$
((f \times) y) \times .
$$

Exercici: Definir una funció que donats un pendent i un valor de tall amb l'eix d'ordenades retorna una funció que denoti l'equació de la recta. Definir una funció que retorni una funció que denoti l'equació de totes les possibles rectes de pendent 4.

## 3 Tipus

Un tipus de dades és un conjunt de valors, i la sintaxi bàsica de definició és la següent (els noms dels constructors comencen amb majúscules):

$$
\text { data }<\text { ConstructorTipus }>=\left.<\text { ConstructorValor }_{1}>\mid \ldots \mid<\text {ConstructorValor }_{S}>\right.
$$

Per exemple, el tipus booleà es defineix com (ja està predefinit a la llibreria bàsica)

## data Bool = False | True

Amb aquesta definició el conjunt de valors del tipus Bool és \{False, True\}.
Per a indicar el tipus d'un valor v s'utilitza la notació v :: T, on T és el constructor del tipus, per exemple True : : Bool. Tota expressió té un tipus i en cas de no indicar-se, el compilador l'infereix. Si els tipus indicats i/o la inferència no encaixa es generen errors de compilació.
Exercici: Definir el tipus M3 que conté tres valors M3 $=\{\mathrm{A}, \mathrm{B}, \mathrm{C}\}$.
Exercici: Sigui la funció

$$
\begin{aligned}
& \text { cert }:: a \rightarrow b \rightarrow a \\
& \text { cert } \mathrm{x} y=x
\end{aligned}
$$

Definir la funció fals i l'operador (funció) d'ordre (.<. ) tal que representi l'ordre $\mathrm{A}<\mathrm{B}<\mathrm{C}$, és a dir, per exemple A .<. B = cert i C .<. B = fals. Tipificar la funció (.<.)
Exercici: Definir la funció maxim :: M3 -> M3 -> M3 -> M3, que donats tres valors de tipus M3 retorna el màxim. Si en la definició que has fet es calcula un mateix valor dues vegades redefinir la funció de dues maneres:

- Fent servir una expressió let ... in.
- Fent servir un bloc where.
