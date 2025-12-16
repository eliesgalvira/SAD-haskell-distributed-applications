{-# LANGUAGE ScopedTypeVariables #-}
module Lab8 where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (finally, SomeException, try, bracket)
import Control.Monad (forever, when, unless, void)
import qualified Data.Map as M
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(..))

-- Secció 2: Threads

-- Lock global per a escriptura atòmica (es crea un cop)
type PrintLock = MVar ()

-- Crear un nou lock per imprimir
newPrintLock :: IO PrintLock
newPrintLock = newMVar ()

-- Escriptura atòmica per evitar interleaving de sortides
-- Utilitza un MVar com a lock
atomicPutStrLnWith :: PrintLock -> String -> IO ()
atomicPutStrLnWith lock s = do
    takeMVar lock        -- Agafar el lock (bloqueja si està ocupat)
    putStrLn s
    hFlush stdout
    putMVar lock ()      -- Alliberar el lock

-- Versió simple (crea el seu propi lock intern - NO recomanat per múltiples threads)
atomicPutStrLn :: String -> IO ()
atomicPutStrLn s = putStrLn s >> hFlush stdout

-- Retardar s segons
retardar :: Int -> IO ()
retardar s = threadDelay $ s * 1000000

-- Tasca que retarda s segons
tascaSegons :: Int -> IO ()
tascaSegons s = do
    atomicPutStrLn $ "Inici tasca retardar " ++ show s ++ " segons."
    retardar s

-- Dues tasques seqüencials
sequencials2 :: IO ()
sequencials2 = do
    tascaSegons 4
    putStrLn "finalitzada tasca 4 segons"
    do
        putStrLn "10 segons mes"
        tascaSegons 10
        putStrLn "finalitzada tasca 10 segons mes"

-- Tasca que llança una excepció
tascaExcepcio :: IO ()
tascaExcepcio = do
    error "Excepcio MEVA!!"
    putStrLn "NO surt!"

-- Seqüencial amb excepció (la segona part no s'executa)
sequencials2Excepcio :: IO ()
sequencials2Excepcio = do
    tascaExcepcio
    do
        putStrLn "10 segons mes"
        tascaSegons 10
        putStrLn "finalitzada tasca 10 segons mes"

-- Secció 2.1: Ús de finally

-- Usant finally: la segona part s'executa sempre
finallySequencials2 :: IO ()
finallySequencials2 = finally
    (do
        tascaSegons 4
        putStrLn "finalitzada tasca 4 segons"
    )
    (do
        putStrLn "10 segons mes"
        tascaSegons 10
        putStrLn "finalitzada tasca 10 segons mes"
    )

-- Finally amb excepció: la segona part s'executa i es propaga l'excepció
finallySequencials2Excepcio :: IO ()
finallySequencials2Excepcio = finally
    tascaExcepcio
    (do
        putStrLn "Ara s'executa"
        putStrLn "10 segons mes"
        tascaSegons 10
        putStrLn "finalitzada tasca 10 segons mes"
    )

-- forkFinally: captura l'excepció sense propagar-la
forkFinallyExcepcio :: IO ()
forkFinallyExcepcio = do
    _ <- forkFinally
        tascaExcepcio
        (\_ -> do
            putStrLn "10 segons mes"
            tascaSegons 10
            putStrLn "finalitzada tasca 10 segons mes"
        )
    retardar 20

-- Secció 2.2: Finalització de threads

-- Mostra com finalitzar un thread des d'un altre
finalitzacioThreads :: IO ()
finalitzacioThreads = do
    -- Crear un thread
    idenThread1 <- forkIO $ do
        tascaSegons 120
        putStrLn "Fi tascaSegons 120"
    -- Quan acabi la següent tasca, finalitzarà el thread idenThread1
    finally
        (do
            tascaSegons 10
            putStrLn "tascaSegons 10 acabada"
        )
        (do
            putStrLn "finalitzar altre thread"
            killThread idenThread1
        )

-- Secció 2.3: Exercici - Dos threads amb timeout

-- Exercici: Thread 1 demana text, Thread 2 el mata als 5 segons
exerciciDosThreads :: IO ()
exerciciDosThreads = do
    printLock <- newPrintLock
    atomicPutStrLnWith printLock "Tens 5 segons per escriure alguna cosa..."
    hSetBuffering stdin LineBuffering
    
    -- MVar per comunicar el resultat
    resultat <- newEmptyMVar
    
    -- Thread 1: demana text
    threadTeclat <- forkIO $ do
        takeMVar printLock
        putStr "Entra text: "
        hFlush stdout
        putMVar printLock ()
        text <- getLine
        tryPutMVar resultat (Just text)
        return ()
    
    -- Thread 2: espera 5 segons i mata el primer
    threadTimer <- forkIO $ do
        retardar 5
        atomicPutStrLnWith printLock "\nTemps esgotat!"
        killThread threadTeclat
        tryPutMVar resultat Nothing
        return ()
    
    -- Esperar el resultat
    res <- takeMVar resultat
    killThread threadTimer  -- Per si l'usuari ha escrit abans
    case res of
        Just text -> atomicPutStrLnWith printLock $ "Has escrit: " ++ text
        Nothing   -> atomicPutStrLnWith printLock "No has escrit res a temps."

-- Secció 3: Canal de Broadcast

-- Tipus per al canal de broadcast
type ChanW a = MVar [Chan a]  -- Extrem d'escriptura (llista de canals de lectura)
type ChanR a = Chan a          -- Extrem de lectura

-- Crea un nou canal de broadcast amb l'extrem d'escriptura
newBroadcastChanW :: IO (ChanW a)
newBroadcastChanW = newMVar []

-- Afegeix un nou extrem de lectura
addChanR :: ChanW a -> IO (ChanR a)
addChanR chanW = do
    nouChan <- newChan
    modifyMVar_ chanW $ \chans -> return (nouChan : chans)
    return nouChan

-- Treu un extrem de lectura
removeChanR :: ChanW a -> ChanR a -> IO ()
removeChanR chanW chanR = do
    modifyMVar_ chanW $ \chans -> return (filter (/= chanR) chans)
  where
    -- Chan no té Eq, així que fem servir una comparació per referència
    -- En realitat, hauríem de guardar identificadors, però simplificarem

-- Nota: La comparació de Chan no és directa. Una implementació més robusta
-- utilitzaria identificadors únics per cada canal.

-- Escriu en l'extrem d'escriptura (broadcast a tots els lectors)
writeChanW :: ChanW a -> a -> IO ()
writeChanW chanW msg = do
    chans <- readMVar chanW
    mapM_ (`writeChan` msg) chans

-- Llegeix d'un extrem de lectura
readChanR :: ChanR a -> IO a
readChanR = readChan

-- Secció 3.1: Exercici Broadcast

-- Exercici amb dos lectors
exerciciBroadcast :: IO ()
exerciciBroadcast = do
    -- Crear lock compartit per a impressió atòmica
    printLock <- newPrintLock
    
    atomicPutStrLnWith printLock "Creant canal de broadcast..."
    chanW <- newBroadcastChanW
    
    -- Afegir dos lectors
    chanR1 <- addChanR chanW
    chanR2 <- addChanR chanW
    
    -- Thread lector 1
    _ <- forkIO $ do
        msg <- readChanR chanR1
        atomicPutStrLnWith printLock $ "Lector 1 ha rebut: " ++ msg
    
    -- Thread lector 2
    _ <- forkIO $ do
        msg <- readChanR chanR2
        atomicPutStrLnWith printLock $ "Lector 2 ha rebut: " ++ msg
    
    -- Donar temps als threads per iniciar-se
    retardar 1
    
    -- Enviar missatge
    atomicPutStrLnWith printLock "Enviant missatge..."
    writeChanW chanW "Hola a tots!"
    
    -- Esperar que els lectors rebin
    retardar 2
    atomicPutStrLnWith printLock "Fi exercici broadcast"

-- Secció 4: Servei Eco

-- Port per defecte
portDefecte :: String
portDefecte = "3000"

-- Tipus per al nick
type Nick = String

-- Dades d'usuari
data Usuari = Usuari
    { nick :: Nick
    , socketUsr :: Socket
    } deriving Show

-- Dades del servidor (amb estat per nicks únics)
data Servidor = Servidor
    { connectats :: MVar (M.Map Nick Usuari)
    }

-- Crear servidor
crearServ :: IO Servidor
crearServ = do
    conn <- newMVar M.empty
    pure $ Servidor conn

-- Secció 4.1: Client Eco bàsic

-- Tasca de xarxa: llegeix del socket i mostra per pantalla
tascaXarxa :: Socket -> MVar Bool -> IO ()
tascaXarxa sock fiMVar = do
    msg <- recv sock 1024
    let text = BS.unpack msg
    if BS.null msg || text == "/fi\n" || text == "/fi"
        then do
            atomicPutStrLn $ "> " ++ text
            atomicPutStrLn "DEBUG: Socket tancat"
            close sock
            putMVar fiMVar True
        else do
            atomicPutStrLn $ "> " ++ text
            tascaXarxa sock fiMVar

-- Tasca de teclat: llegeix del teclat i envia pel socket
tascaTeclat :: Socket -> MVar Bool -> IO ()
tascaTeclat sock fiMVar = do
    resultat <- tryTakeMVar fiMVar
    case resultat of
        Just True -> do
            atomicPutStrLn "INFO: Teclat tancat"
            return ()
        _ -> do
            linia <- getLine
            sendAll sock (BS.pack $ linia ++ "\n")
            if linia == "/fi"
                then do
                    atomicPutStrLn "INFO: Teclat tancat"
                    return ()
                else tascaTeclat sock fiMVar

-- Client general amb dos threads
clientGeneral :: IO ()
clientGeneral = do
    hSetBuffering stdin LineBuffering
    addr <- resolve "127.0.0.1" portDefecte
    bracket (open addr) close $ \sock -> do
        fiMVar <- newEmptyMVar
        
        -- Demanar nick
        putStrLn "> Entra nick:"
        nickUsuari <- getLine
        sendAll sock (BS.pack $ nickUsuari ++ "\n")
        
        -- Esperar resposta del servidor
        resposta <- recv sock 1024
        let text = BS.unpack resposta
        
        if "ja es fa servir" `isInfixOf` text
            then do
                putStrLn $ "> " ++ text
                clientGeneral  -- Tornar a intentar
            else do
                putStrLn $ "> " ++ text
                
                -- Thread per llegir de la xarxa
                _ <- forkIO $ tascaXarxa sock fiMVar
                
                -- Thread principal llegeix del teclat
                tascaTeclat sock fiMVar
                
                atomicPutStrLn "INFO: Client finalitzat"
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
    
    isInfixOf needle haystack = needle `elem` (map (take (length needle)) (tails haystack))
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- Secció 4.3: Servidor Eco amb Nick

-- Obtenir nick d'un client
obtenirNick :: Socket -> IO Usuari
obtenirNick sc = do
    sendAll sc (BS.pack "Entra nick:\n")
    msg <- recv sc 1024
    let nickRebut = filter (/= '\n') $ BS.unpack msg
    atomicPutStrLn $ "INFO: Nick rebut -> < " ++ nickRebut ++ " >"
    sendAll sc (BS.pack $ nickRebut ++ " entra text:\n")
    return $ Usuari nickRebut sc

-- Bucle d'eco
bucleEco :: Usuari -> IO ()
bucleEco usuari = do
    msg <- recv (socketUsr usuari) 1024
    let text = filter (/= '\n') $ BS.unpack msg
    atomicPutStrLn $ "INFO: Rebut de < " ++ nick usuari ++ " > : " ++ text
    if text == "/fi" || BS.null msg
        then do
            atomicPutStrLn $ "INFO: Acabant connexió amb " ++ nick usuari
            sendAll (socketUsr usuari) (BS.pack "/fi\n")
        else do
            sendAll (socketUsr usuari) (BS.pack $ text ++ "\n")
            sendAll (socketUsr usuari) (BS.pack $ nick usuari ++ " entra text:\n")
            bucleEco usuari

-- Tasca del servidor d'eco amb nick
tascaEcoServidorNick :: Socket -> IO ()
tascaEcoServidorNick sc = do
    usuari <- obtenirNick sc
    bucleEco usuari

-- Servidor d'eco simple amb nick
servidorEcoNick :: IO ()
servidorEcoNick = do
    addr <- resolve portDefecte
    bracket (open addr) close $ \sock -> do
        atomicPutStrLn $ "INFO: Escoltant pel port " ++ portDefecte
        bucleAcceptar sock
  where
    resolve port = do
        let hints = defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
        head <$> getAddrInfo (Just hints) Nothing (Just port)
    
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 5
        return sock
    
    bucleAcceptar sock = do
        (conn, peer) <- accept sock
        atomicPutStrLn $ "INFO: Connexió acceptada desde " ++ show peer
        _ <- forkIO $ finally
            (tascaEcoServidorNick conn)
            (do
                atomicPutStrLn $ "INFO: Connexió acabada desde " ++ show peer
                atomicPutStrLn "DEBUG: Socket tancat"
                close conn
            )
        bucleAcceptar sock

-- Secció 4.4: Servidor Eco amb Nick Únic

-- Intenta afegir un nou usuari al mapa del servidor
-- Retorna Just usuari si no existeix, Nothing si ja existeix
afegirUsuari :: Nick -> Socket -> Servidor -> IO (Maybe Usuari)
afegirUsuari nom socket servidor = do
    let nouUsuari = Usuari nom socket
    modifyMVar (connectats servidor) $ \mapa ->
        if M.member nom mapa
            then do
                atomicPutStrLn $ "DEBUG: Usuari " ++ nom ++ " ja existeix"
                return (mapa, Nothing)
            else do
                atomicPutStrLn $ "DEBUG: Usuari " ++ nom ++ " afegit"
                return (M.insert nom nouUsuari mapa, Just nouUsuari)

-- Treu l'usuari del mapa del servidor
treureUsuari :: Usuari -> Servidor -> IO ()
treureUsuari usuari servidor = do
    modifyMVar_ (connectats servidor) $ \mapa ->
        return $ M.delete (nick usuari) mapa
    atomicPutStrLn $ "DEBUG: Usuari " ++ nick usuari ++ " eliminat"

-- Obtenir nick únic (demana fins que sigui únic)
obtenirNickUnic :: Servidor -> Socket -> IO Usuari
obtenirNickUnic servidor sc = do
    sendAll sc (BS.pack "Entra nick:\n")
    msg <- recv sc 1024
    let nickRebut = filter (/= '\n') $ BS.unpack msg
    atomicPutStrLn $ "INFO: Nick rebut -> < " ++ nickRebut ++ " >"
    
    resultat <- afegirUsuari nickRebut sc servidor
    case resultat of
        Just usuari -> do
            sendAll sc (BS.pack $ nickRebut ++ " entra text:\n")
            return usuari
        Nothing -> do
            sendAll sc (BS.pack $ "El nick " ++ nickRebut ++ " ja es fa servir, tria'n un altre\n")
            obtenirNickUnic servidor sc

-- Tasca del servidor d'eco amb nick únic
tascaEcoServidorNickUnic :: Servidor -> Socket -> IO ()
tascaEcoServidorNickUnic servidor sc = do
    usuari <- obtenirNickUnic servidor sc
    finally
        (bucleEco usuari)
        (treureUsuari usuari servidor)

-- Servidor d'eco amb nick únic
servidorEcoNickUnic :: IO ()
servidorEcoNickUnic = do
    servidor <- crearServ
    addr <- resolve portDefecte
    bracket (open addr) close $ \sock -> do
        atomicPutStrLn $ "INFO: Escoltant pel port " ++ portDefecte
        bucleAcceptar servidor sock
  where
    resolve port = do
        let hints = defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
        head <$> getAddrInfo (Just hints) Nothing (Just port)
    
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 5
        return sock
    
    bucleAcceptar servidor sock = do
        (conn, peer) <- accept sock
        atomicPutStrLn $ "INFO: Connexió acceptada desde " ++ show peer
        _ <- forkIO $ finally
            (tascaEcoServidorNickUnic servidor conn)
            (do
                atomicPutStrLn $ "INFO: Connexió acabada desde " ++ show peer
                atomicPutStrLn "DEBUG: Socket tancat"
                close conn
            )
        bucleAcceptar servidor sock

-- Main

main :: IO ()
main = do
    putStrLn "Lab 8 - Threads, Broadcast Channels, Echo Service"
    putStrLn "Funcions disponibles:"
    putStrLn "  - sequencials2, finallySequencials2"
    putStrLn "  - finalitzacioThreads"
    putStrLn "  - exerciciDosThreads"
    putStrLn "  - exerciciBroadcast"
    putStrLn "  - servidorEcoNick, servidorEcoNickUnic"
    putStrLn "  - clientGeneral"


