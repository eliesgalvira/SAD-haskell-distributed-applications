module Clases.Fitxers.MonadEstat where

import Data.Char
import Data.List
import System.IO
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Traversable
import System.Console.ANSI
-- stack install ansi-terminal
import Control.Monad


-------------------------------------------------------------------
-------------------------------------------------------------------
-----------------------    Joc Mostra     -------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

data EstatEnd = E {lletra :: Char, numIntents :: Int}

{-
    Monad Estat per sobre monad IO

    newtype StateT s m a
    A state transformer monad parameterized by:
       - s : The state.
       - m : The inner monad.
    Constructor: StateT (s -> m (a, s))
-}
type Endevinar = StateT EstatEnd IO ()


estatEnd = E {lletra = 'h', numIntents = 0}

mostrarIntents :: Int -> String
mostrarIntents 0 = ""
mostrarIntents n = "x " ++ (mostrarIntents (n-1))

jocEnd :: Endevinar
jocEnd = do
    -- per fer operacions IO faig servir lift
    lift (putStr "Entra lletra: ")
    jugada <- lift getLine
    -- get: obte l'estat actual
    est <- get
    if (head jugada == lletra est) then lift (putStrLn "Has guanyat") 
    else do
        -- put modifica l'estat
        put $ E {lletra = lletra est, numIntents = (numIntents est)+ 1} 
        est <- get
        lift (putStrLn ("num intents : "++(mostrarIntents (numIntents est))))
        jocEnd



jugarEnd = runStateT jocEnd estatEnd

{- 
evalStateT :: Monad m => StateT s m a -> s -> m a

    Evaluate a state computation with the given initial state 
    and return the final value, discarding the final state.
-}

jugarEnd' = evalStateT jocEnd estatEnd
-------------------------------------------------------------------
-------------------------------------------------------------------
------------------         Moviment            --------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

-- Pos es (files, columnes)
type Pos = (Int,Int)


incFila (x, y) = (x + 1, y)
decFila (x, y) = (x - 1, y)
incCol (x, y) = (x, y + 1)
decCol (x, y) = (x, y - 1)


anar :: Pos -> IO ()
anar (x,y) = setCursorPosition x y


type Moviment = StateT [Pos] IO ()

{- acccio que:
    - a partir d'una funcio donada,
      obte una nova posicio, 
    - l'afegeix a l'estat
    - dibuixa una 'x' en aquesta nova posicio
 - per aixo: 
   - obte l'estat actual
   - accedeix a l'ultima posicio
   - crea una nova posicio aplicant la funcio
     a l'ultima posicio
   - canvia l'estat afegint la nova posicio
     al final de la llista
   - fa servir la funcio dibuixaMarca per dibuixar
     una 'x' en aquesta ultima posicio
-}
mouIdibuixa :: (Pos-> Pos) -> Moviment
mouIdibuixa canviPos = do
    est <- get
    let ultimaPos = last est
        novaPos   = canviPos ultimaPos
    put (est ++ [novaPos])
    lift $ dibuixaMarca novaPos

-- 'd' : opcio dreta
-- 'e' : opcio esquerra
-- 'm' : opcio amunt
-- 'v' : opcio avall
-- en qualsevol altre cas: opcio buida
opcio :: Char -> Moviment
opcio c = case c of
    'd' -> mouIdibuixa incCol
    'e' -> mouIdibuixa decCol
    'm' -> mouIdibuixa decFila
    'v' -> mouIdibuixa incFila
    _   -> cap


-- Dibuixa una 'x' en la posicio Pos
dibuixaMarca :: Pos -> IO ()
dibuixaMarca pos = do
    anar pos
    putChar 'x'

{-
(>>) :: m a -> m b -> m b

    Sequentially compose two actions, 
    discarding any value produced by the first, 
    like sequencing operators (such as the semicolon) 
    in imperative languages.

    'as >> bs' can be understood as the do expression
    do
        as
        bs
-}

{-
Quins son els tipus de les seguents expressions?:
  - mapM_ :: (a -> m b) -> t a -> m ()
  . opcio :: Char -> Moviment
  . entrada :: [Char]
  - (mapM_ opcio entrada) :: Moviment
  . evalStateT :: Monad m => StateT s m a -> s -> m a
  - evalStateT (mapM_ opcio entrada) :: IO ()
  - final :: Moviment
  - clearScreen :: IO ()
-}

-- entrada prova
entrada = "ddddddddddddddvvvvvvvvvveeeeeeeeeeemmmmmmddddddvvvee"

{-
Accio StateT [Pos] IO () que dibuixa el moviment provocat per 
'entrada'
-}
accioMovEntrada :: StateT [Pos] IO ()
accioMovEntrada = mapM_ opcio entrada

{-
Accio IO () que:
    - neteja la pantalla
    - executa accioMovEntrada
-}
dibuixarEntrada :: IO ()
dibuixarEntrada = do
    clearScreen
    evalStateT (accioMovEntrada >> final) [(0,0)]

{-
Sortida:

 xxxxxxxxxxxxxx
              x
              x
              x
   xxxxxxx    x
   x     x    x
   x     x    x
   x   xxx    x
   x          x
   x          x
   xxxxxxxxxxxx

ghci> 
-}

---------------------------------------------------------------------
-------------------     Funcions utils     --------------------------
---------------------------------------------------------------------


-- accio buida
cap :: Moviment
cap = pure ()


-- accio que situa cal cursor al final 
-- del dibuix
final :: Moviment
final = do
    est <- get
    lift $ anar (maxX est + 2, 0)


-- foldl :: (a -> b -> a) -> a -> [b] -> a
maxX xs = foldl aux ((fst . head) xs) xs
    where aux max (x,y)
            | x > max = x
            | otherwise = max


-------------------------------------------------------------------
-------------------------------------------------------------------
------------------    Moviment Interactiu      --------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

-- prova d'executar el seguent monad i observa els problemes que te
movProva :: Moviment
movProva = do
    jugada <- lift getChar
    opcio jugada
    movProva


-- donada una llista de Pos,
-- obtenir una accio d'IO que 
-- dibuixa una 'x' en cada una de
-- les posicions de la llista
-- fer servir mapM_
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mostrar :: [Pos] -> IO ()
mostrar = mapM_ dibuixaMarca

{- acccio que:
    - a partir d'una funcio donada,
      obte una nova posicio, 
    - l'afegeix a l'estat
    - redibuixa tot l'estat, dibuxant
      una 'x' en cada posicio
 - per aixo: 
   - obte l'estat actual
   - accedeix a l'ultima posicio
   - crea una nova posicio aplicant la funcio
     a l'ultima posicio
   - canvia l'estat afegint la nova posicio
     al final de la llista
   - redibuixa la llista de posicions
-}
mouIdibuixaI ::  (Pos-> Pos) -> Moviment
mouIdibuixaI canviPos = do
    est <- get
    let ultimaPos = last est
        novaPos   = canviPos ultimaPos
        nouEst    = est ++ [novaPos]
    put nouEst
    lift $ mostrar nouEst


-- 'd' : opcio dreta
-- 'e' : opcio esquerra
-- 'm' : opcio amunt
-- 'v' : opcio avall
-- en qualsevol altre cas: opcio buida
opcioI :: Char -> Moviment
opcioI c = case toLower c of
    'd' -> mouIdibuixaI incCol
    'e' -> mouIdibuixaI decCol
    'm' -> mouIdibuixaI decFila
    'v' -> mouIdibuixaI incFila
    _   -> cap



{-
Acció que:
    - Demana un sentit (d,e,v,m,f) a l'usuari
    - neteja la consola
    - afegeix a l'estat la nova posicio
      ( i redibuixa )
    - situa el cursor al final del dibuix
    - torna a demanar un sentit 

sera util:
    when :: Applicative f => Bool -> f () -> f ()
        Conditional execution of Applicative expressions
-}
movimentI :: Moviment
movimentI = do
    lift clearScreen
    est <- get
    lift $ mostrar est
    final
    bucle
  where
    bucle = do
        lift $ putStr "Sentit (d,e,v,m,f): "
        entrada <- lift getLine
        let jugada = toLower (if null entrada then '\n' else head entrada)
        when (jugada /= 'f') $ do
            when (jugada `elem` ['d','e','m','v']) $ do
                lift clearScreen
                opcioI jugada
                final
            bucle


-- evaluar l'accio movimentI
moureI = evalStateT movimentI [(0,0)]
