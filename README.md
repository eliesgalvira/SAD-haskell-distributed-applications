## SAD — Software per a Aplicacions Distribuïdes

### Descripció
Assignatura pràctica on implementem elements de protocols de comunicació (p. ex. TCP i HTTP) utilitzant Haskell. L’èmfasi és en entendre a fons els mecanismes de comunicació i reflectir-los en codi funcional clar, segur i testejable.

### Objectius d’aprenentatge
- **Protocols i xarxa**: comprendre el model de sockets, el comportament de TCP (connexió, fiabilitat, flux) i la semàntica d’HTTP (request/response, mètodes, codis d’estat).
- **Programació funcional en Haskell**: funcions pures, tipus i tipificat, composició, aplicació parcial i funcions d’ordre superior.
- **Enginyeria de programari**: organització de projecte, ús d’eines (Stack, GHCi), proves i verificació de propietats quan escaigui.

### Estructura del repositori
- **`lab1/`**: repàs de fonaments de Haskell i tipificat.
  - `inici.hs`: primer contacte amb GHCi i definició de funcions simples.
  - `2-funcions.hs`: funcions, lambda, infixa, ordre superior, aplicació parcial.
  - `3-tipus.hs`: definició i ús de tipus propis.
  - `lab1-tasques.md`: enunciat i guió d’exercicis del laboratori 1.

Mesures i laboratoris posteriors (TCP, HTTP, concurrència, etc.) s’afegiran amb nous directoris `labN/` a mesura que avanci el curs.

### Requisits
- **Stack** per gestionar el toolchain i sessions de GHCi. Vegeu la web de [Stack](https://haskellstack.org).
- Alternativament, **GHC** i **GHCi** instal·lats (p. ex. via [GHCup](https://www.haskell.org/ghcup/)).
- Entorn Linux/Unix recomanat (el repositori s’ha provat en Linux).

### Posada en marxa ràpida
1) Obrir un terminal al directori del projecte.

2) Llençar GHCi sobre un fitxer de laboratori (exemple `lab1/inici.hs`):
```bash
stack ghci lab1/inici.hs
```

3) Al prompt `ghci>`, executar funcions i re-carregar el fitxer quan editeu:
```text
ghci> inc 4
ghci> :reload   -- o bé :r
```

4) També podeu interpretar un fitxer puntual sense obrir una sessió GHCi persistent:
```bash
stack runghc lab1/inici.hs
```

Consell: manteniu GHCi obert mentre codeu i useu `:r` per a bucle curt d’edició–execució.

### Bones pràctiques
- **Tipifiqueu** sempre les funcions exportades; facilita la lectura i els errors de tipus precoços.
- **Descomposeu** en funcions petites i pures; faciliteu proves i raonament.
- **Afegiu proves** o propietats (p. ex. QuickCheck) quan hi hagi lògica no trivial.

### Recursos útils
- [Stack — Documentació oficial](https://docs.haskellstack.org/en/stable/README/)
- [GHCup — Instal·lació de GHC i eines](https://www.haskell.org/ghcup/)
- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
- [Haskell Wiki](https://wiki.haskell.org/Haskell)

# Nix
`nix-shell`
`cabal test --test-show-detail=streaming`
