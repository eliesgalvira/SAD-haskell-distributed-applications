module Clases.Fitxers.ThreadsMVars where

import Control.Concurrent
import Text.Printf
import Control.Monad


-- Per a crear un fil
-- forkIO :: IO () -> IO ThreadId

primerAB :: IO ()
primerAB = do 
            forkIO $ putStrLn "A"
            forkIO $ putStrLn "B"
            pure ()

-- Bucle que imprimeix N vegades un string per pantalla
bucleMostrar :: Int -> String -> IO()
bucleMostrar 0 _ = pure ()
bucleMostrar n str = do
                        putStr str
                        bucleMostrar (n-1) str

-- bucle que imprimeix 1000 A i un altre que imprimeix 1000 B
segonAB :: IO ()
segonAB = do
            forkIO $ bucleMostrar 1000 "A"
            forkIO $ bucleMostrar 1000 "B"
            pure ()


{-
Les MVar són el mecanisme bàsic de comunicació que ofereix Haskell,
és a dir, les variables globals bàsiques.

data MVar a = ...

newEmptyMvar :: IO (MVar a)

newMVar :: a -> IO (MVar a)

takeMVar :: MVar a -> IO a

    - Retorna el contingut de l'MVar. 
    
    - Si l'MVar està buida, takeMVar esperarà fins que estigui plena.
      Després d'un takeMVar, l'MVar es queda buida.

putMVar :: MVar a -> a -> IO ()

    - Posa un valor a un MVAR. 
    
    - Si l'MVar és plena, putMvar esperarà fins que es buidi.


Propietats de takeMVar i putMVar:
        
        - Només desperten UN ÚNIC thread. És a dir, si hi ha diversos 
          fils bloquejats a putMVar/takeMVar al canviar l'estat només 
          se'n despertarà un. 
        
        - Quan es bloquegen diversos fils en un MVAR, 
          es desperten en ordre FIFO.


readMVar :: MVar a -> IO a

    - Llegeix atòmicament el contingut d'una MVAR. 
    
    - Si l'MVar és buida, readMVar esperarà fins que estigui plena. 

    - Si hi ha varis threads bloquejats en una MVar, 
      readMVar els despertarà tots al mateix temps.


- Les MVAR són variables que poden estar buides o  contenir un valor.

- Les operacions de posar i treure el valor són bloquejants.

- Les operacions sempre retornen valors del monad IO. 
-}

-- MVar per a compartir valors entre fils

pasValors :: IO ()
pasValors = do
    mvar <- newEmptyMVar

    forkIO $ do
        putMVar mvar 0
        putMVar mvar 1

    r <- takeMVar mvar
    print r
    r <- takeMVar mvar
    print r
    pure ()

tercerAB :: IO ()
tercerAB = undefined




{-
Fer que el fil principal s'esperi a que la resta de fils acabin. 
La idea es disposar d'una MVar on cada fil posa un valor un cop 
ha acabat la seva feina i el fil principal va obtenint valors 
d'aquesta MVar, tants com fils s'han iniciat.
-}

type CF = MVar ()


iniciarFil :: IO () -> IO CF
iniciarFil accio = undefined


--  where
    -- Delega l'execució de la funció del fil a una nova funció, 
    -- que executa la funció del fil i després actualitza l'MVar.



esperarFil :: CF -> IO ()
esperarFil = undefined


{-
forM :: Monad m => [a] -> (a -> m b) -> m [b]
    - Map each element of a structure to a monadic action, 
    - evaluate these actions from left to right, 
    - and collect the results. 
-}
iniciarFils :: [IO ()] -> IO [CF]
iniciarFils = undefined



{-
replicate :: Int -> a -> [a]
    
    - replicate n x 
      
      is a list of length n with x the value of every element.
-}
iniciarFilsIguals :: Int -> IO () -> IO [CF]
iniciarFilsIguals = undefined




{-
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
    - Map each element of a structure to a monadic action, 
    - evaluate these actions from left to right, 
    - and ignore the results. 
-}
esperarFils :: [CF] -> IO ()
esperarFils = undefined



meuFun :: String -> IO ()
meuFun str = do
    putStrLn str 



primerFils :: IO ()
primerFils = undefined



type ControlPrint = MVar ()

atomicPutStrLn :: ControlPrint -> String -> IO ()
atomicPutStrLn lock str = undefined
            


segonFils :: IO ()
segonFils = undefined



{-
Problema clàssic:

  'numFils' fils incrementant una variable 'numInc' cops cada fil. 

Resoldre el problema posant aquesta variable en una MVar.
-}

type Comptador = MVar Int

inc :: Comptador -> IO ()
inc c = undefined



filInc :: Int -> Comptador -> IO ()
filInc = undefined



exempleComptador :: Int -> Int -> IO ()
exempleComptador numFils numInc = undefined





-- Productors - Consumidors

{-
when :: Applicative f => Bool -> f () -> f ()

Execució condicional d'expressions Applicative. 

Exemples:
    
    when debug (putStrLn "Debugging")

  obtindrà a la sortida:
      -  l'string Debugging si el valor booleà 'debug' és True
      -  sino no és farà res.

ghci> putStr "pi:" >> when False (print 3.14159)
pi:

ghci> putStr "pi:" >> when True (print 3.14159) 
pi:3.14159

-}





data BufferPC a = B {cua :: (MVar ([a], Int)), control :: ControlPC}
data ControlPC = C {cp :: MVar (), cc::MVar ()}

controlPC :: IO ControlPC
controlPC = undefined



nouBufferPC :: Int -> IO (BufferPC a)
nouBufferPC capacitat = undefined




posarPC :: Show a => ControlPrint -> BufferPC a -> a -> IO ()
posarPC cprt buf  val  = undefined





treurePC ::  (Show a) => ControlPrint -> BufferPC a -> IO a
treurePC cprt buf = undefined




elems :: [Char]
elems = ['a','b','c','d']

productor :: (Show a) => ControlPrint -> [a] -> BufferPC a ->  IO ()
productor = undefined




consumidor :: (Show a) => ControlPrint ->  Int -> BufferPC a -> IO ()
consumidor = undefined




exemplePC :: IO ()
exemplePC = undefined

    


