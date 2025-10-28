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

### Executar amb Nix (ara: "Hello World")
Per ara, `nix run` imprimeix "Hello World". Més endavant, aquest comandament executarà el client TCP del projecte.

- **Des del repositori clonat:**
```bash
nix run
```

- **Directament a traves de GitHub:**
```bash
nix run github:eliesgalvira/SAD-haskell-distributed-applications
```

Si el vostre sistema tenia una versió antiga en memòria cau, forceu refresc:
```bash
nix run --refresh github:eliesgalvira/SAD-haskell-distributed-applications
```

### Instal·lació ultra-ràpida per sistema operatiu
1) **Linux/macOS** — instal·lar Nix en un pas:
```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```
2) A continuació, executar el projecte:
```bash
nix run github:eliesgalvira/SAD-haskell-distributed-applications
```

1) **Windows** — recomanat via WSL (Windows Subsystem for Linux):
   - Obriu PowerShell (Admin) i instal·leu WSL (un cop):
```powershell
wsl --install -d Ubuntu
```
   - A la terminal d'Ubuntu que s'obrirà, executeu els passos de Linux (instal·lar Nix i `nix run`).

Alternatives per Windows: utilitzar un entorn Linux (p. ex. VM) o un servei remot.

### Entorn de desenvolupament amb Nix
Obrir un shell amb GHC i `cabal` preinstal·lats:
```bash
nix develop
```

### Proves amb Cabal
- **Directament (fora o dins del dev shell):**
```bash
cabal test --test-show-detail=streaming
```

- **Amb Nix (una sola ordre):**
```bash
nix develop -c cabal test --test-show-detail=streaming
```

> Nota: al dia a dia podeu seguir usant GHCi per a fitxers concrets (vegeu "Posada en marxa ràpida"). `cabal test` s'utilitza sobretot per orquestrar proves del laboratori.
