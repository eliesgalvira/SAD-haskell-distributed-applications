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

-- Versió amb MVar per sincronitzar A i B alternats
tercerAB :: IO ()
tercerAB = do
    mvar <- newEmptyMVar
    forkIO $ do
        bucleMostrar 100 "A"
        putMVar mvar ()  -- Senyalitza que A ha acabat
    takeMVar mvar        -- Espera que A acabi
    forkIO $ bucleMostrar 100 "B"
    pure ()




{-
Fer que el fil principal s'esperi a que la resta de fils acabin. 
La idea es disposar d'una MVar on cada fil posa un valor un cop 
ha acabat la seva feina i el fil principal va obtenint valors 
d'aquesta MVar, tants com fils s'han iniciat.
-}

type CF = MVar ()


iniciarFil :: IO () -> IO CF
iniciarFil accio = do
    cf <- newEmptyMVar
    forkIO $ do
        accio                -- Executa l'acció del fil
        putMVar cf ()        -- Senyalitza que ha acabat
    pure cf

--  where
    -- Delega l'execució de la funció del fil a una nova funció, 
    -- que executa la funció del fil i després actualitza l'MVar.



esperarFil :: CF -> IO ()
esperarFil cf = takeMVar cf  -- Bloqueja fins que el fil acabi


{-
forM :: Monad m => [a] -> (a -> m b) -> m [b]
    - Map each element of a structure to a monadic action, 
    - evaluate these actions from left to right, 
    - and collect the results. 
-}
iniciarFils :: [IO ()] -> IO [CF]
iniciarFils accions = forM accions iniciarFil



{-
replicate :: Int -> a -> [a]
    
    - replicate n x 
      
      is a list of length n with x the value of every element.
-}
iniciarFilsIguals :: Int -> IO () -> IO [CF]
iniciarFilsIguals n accio = iniciarFils (replicate n accio)




{-
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
    - Map each element of a structure to a monadic action, 
    - evaluate these actions from left to right, 
    - and ignore the results. 
-}
esperarFils :: [CF] -> IO ()
esperarFils cfs = forM_ cfs esperarFil



meuFun :: String -> IO ()
meuFun str = do
    putStrLn str 



primerFils :: IO ()
primerFils = do
    cfs <- iniciarFils [meuFun "Fil 1", meuFun "Fil 2", meuFun "Fil 3"]
    esperarFils cfs
    putStrLn "Tots els fils han acabat!"



type ControlPrint = MVar ()

atomicPutStrLn :: ControlPrint -> String -> IO ()
atomicPutStrLn lock str = do
    takeMVar lock      -- Agafar el lock (bloqueja si ocupat)
    putStrLn str       -- Imprimir
    putMVar lock ()    -- Alliberar el lock
            


segonFils :: IO ()
segonFils = do
    lock <- newMVar ()  -- Crear lock per impressió atòmica
    cfs <- iniciarFils 
        [ atomicPutStrLn lock "Fil 1"
        , atomicPutStrLn lock "Fil 2"
        , atomicPutStrLn lock "Fil 3"
        ]
    esperarFils cfs
    atomicPutStrLn lock "Tots els fils han acabat!"



{-
Problema clàssic:

  'numFils' fils incrementant una variable 'numInc' cops cada fil. 

Resoldre el problema posant aquesta variable en una MVar.
-}

type Comptador = MVar Int

inc :: Comptador -> IO ()
inc c = do
    val <- takeMVar c      -- Agafar valor (i bloquejar)
    putMVar c (val + 1)    -- Posar valor incrementat (i desbloquejar)



filInc :: Int -> Comptador -> IO ()
filInc n c = replicateM_ n (inc c)  -- Incrementar n vegades



exempleComptador :: Int -> Int -> IO ()
exempleComptador numFils numInc = do
    comptador <- newMVar 0
    cfs <- iniciarFilsIguals numFils (filInc numInc comptador)
    esperarFils cfs
    resultat <- takeMVar comptador
    printf "Resultat: %d (esperat: %d)\n" resultat (numFils * numInc)





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
controlPC = do
    cpVar <- newMVar ()      -- Productor pot començar (hi ha espai)
    ccVar <- newEmptyMVar    -- Consumidor ha d'esperar (no hi ha res)
    pure $ C cpVar ccVar



nouBufferPC :: Int -> IO (BufferPC a)
nouBufferPC capacitat = do
    cuaVar <- newMVar ([], capacitat)  -- Cua buida amb capacitat
    ctrl <- controlPC
    pure $ B cuaVar ctrl




posarPC :: Show a => ControlPrint -> BufferPC a -> a -> IO ()
posarPC cprt buf val = do
    -- Esperar si el buffer està ple
    (llista, cap) <- takeMVar (cua buf)
    if length llista >= cap
        then do
            putMVar (cua buf) (llista, cap)  -- Tornar a posar
            takeMVar (cp (control buf))       -- Esperar senyal de consumidor
            posarPC cprt buf val              -- Tornar a intentar
        else do
            let novaLlista = llista ++ [val]
            putMVar (cua buf) (novaLlista, cap)
            atomicPutStrLn cprt $ "Productor: afegit " ++ show val
            -- Senyalitzar al consumidor que hi ha element
            _ <- tryPutMVar (cc (control buf)) ()
            pure ()





treurePC ::  (Show a) => ControlPrint -> BufferPC a -> IO a
treurePC cprt buf = do
    (llista, cap) <- takeMVar (cua buf)
    case llista of
        [] -> do
            putMVar (cua buf) (llista, cap)  -- Tornar a posar
            takeMVar (cc (control buf))       -- Esperar senyal de productor
            treurePC cprt buf                 -- Tornar a intentar
        (x:xs) -> do
            putMVar (cua buf) (xs, cap)
            atomicPutStrLn cprt $ "Consumidor: tret " ++ show x
            -- Senyalitzar al productor que hi ha espai
            _ <- tryPutMVar (cp (control buf)) ()
            pure x




elems :: [Char]
elems = ['a','b','c','d']

productor :: (Show a) => ControlPrint -> [a] -> BufferPC a ->  IO ()
productor cprt elements buf = forM_ elements $ \e -> do
    posarPC cprt buf e
    threadDelay 100000  -- Petit retard per veure l'efecte




consumidor :: (Show a) => ControlPrint ->  Int -> BufferPC a -> IO ()
consumidor cprt n buf = replicateM_ n $ do
    _ <- treurePC cprt buf
    threadDelay 150000  -- Petit retard (consumidor més lent)




exemplePC :: IO ()
exemplePC = do
    lock <- newMVar ()           -- Lock per impressió atòmica
    buf <- nouBufferPC 2         -- Buffer amb capacitat 2
    
    atomicPutStrLn lock "Iniciant productor-consumidor..."
    
    -- Iniciar productor i consumidor
    cfProd <- iniciarFil $ productor lock elems buf
    cfCons <- iniciarFil $ consumidor lock (length elems) buf
    
    -- Esperar que acabin
    esperarFil cfProd
    esperarFil cfCons
    
    atomicPutStrLn lock "Fi productor-consumidor!"

    


