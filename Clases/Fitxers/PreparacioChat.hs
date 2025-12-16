import Control.Concurrent
import Control.Exception
import System.Random
import Control.Monad
import System.IO.Unsafe
import Data.List (delete)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M


controlPrint :: MVar ()
controlPrint = unsafePerformIO $ newMVar ()

atomicPutStrLn ::  String -> IO ()
atomicPutStrLn str = 
            takeMVar controlPrint>> putStrLn str >> putMVar controlPrint ()



---------------------------------------------------------------------------
------------------------------- Threads -----------------------------------
---------------------------------------------------------------------------

-- retardar s segons
retardar :: Int -> IO ()
retardar s = threadDelay $ s*1000000

tascaSegons :: Int -> IO ()
tascaSegons s = do
                atomicPutStrLn  $ "Inici tasca retardar " ++ (show s) ++ " segons."
                retardar s
                

sequencials2 :: IO ()
sequencials2 = do
                    tascaSegons 4
                    putStrLn  "finalitzada tasca 4 segons"
                    do 
                        putStrLn  "10 segons mes"
                        tascaSegons 10 
                        putStrLn  "finalitzada tasca 10 segons mes"

---------------------------------------------------------------------------
---------------------------- Ús de finally --------------------------------
---------------------------------------------------------------------------

 {-
finally
    :: IO a	 -- computation to run first
    -> IO b	 -- computation to run afterward (even if an exception was raised)
    -> IO a	 
-}           

finallySequencials2 :: IO ()
finallySequencials2 = finally   (do
                                    tascaSegons 4
                                    putStrLn  "finalitzada tasca 4 segons"
                                )
                                (do 
                                    putStrLn "10 segons mes"
                                    tascaSegons 10 
                                    putStrLn  "finalitzada tasca 10 segons mes"
                                )

tascaExcepcio ::  IO ()
tascaExcepcio  = do
                error "Excepcio MEVA!!"
                putStrLn $ "NO surt!"


sequencials2Excepcio :: IO ()
sequencials2Excepcio =  do
                            tascaExcepcio
                            do 
                                putStrLn  "10 segons mes"
                                tascaSegons 10 
                                putStrLn  "finalitzada tasca 10 segons mes"



-- s'executa la segona tasca pero es propaga l'excepcio

finallySequencials2Excepcio :: IO ()
finallySequencials2Excepcio = finally   tascaExcepcio

                                        (do 
                                            putStrLn  "Ara s'executa"
                                            putStrLn  "10 segons mes"
                                            tascaSegons 10 
                                            putStrLn  "finalitzada tasca 10 segons mes"
                                        )


-- forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinallyExcepcio :: IO ()
forkFinallyExcepcio = do 
                _ <- forkFinally    (tascaExcepcio) 
                                    (\ _ -> do
                                        putStrLn $ "10 segons mes"
                                        tascaSegons 10 
                                        putStrLn $ "finalitzada tasca 10 segons mes"
                                    )                    
                retardar 20
                        


---------------------------------------------------------------------------
------------------------ Finalització de Threads --------------------------
---------------------------------------------------------------------------

finalitzacioThreads :: IO ()
finalitzacioThreads = do
    -- Crear un thread
    idenThread1 <- forkIO $ do
                                tascaSegons 120
                                putStrLn "Fi tascaSegons 120 " 
    -- quan acabi la seguent tasca 
    -- fara finalitzar el thread idenThread1
    finally (do
                tascaSegons 10
                putStrLn "tascaSegons 10 acabada")
            (do
                putStrLn "finalitzar altre thread"
                killThread idenThread1
            )





---------------------------------------------------------------------------
------------------------- Canal de Broadcast ------------------------------
---------------------------------------------------------------------------

type ChanW a = MVar [Chan a]
type ChanR a = Chan a

-- crea un nou canal de broadcast amb l'extrem d'escriptura
newBroadcastChanW :: IO (ChanW a)

-- afegeix un nou extrem de lectura
addChanR :: ChanW a -> IO (ChanR a)
-- treu un nou extrem de lectura
removeChanR :: ChanW a -> ChanR a -> IO ()

-- escriu en l'extrem d'escriptura
writeChanW :: ChanW a -> a -> IO ()
-- llegeix d'un extrem de lectura
readChanR :: ChanR a -> IO a

newBroadcastChanW = newMVar []

addChanR chw = do
    chrs <- takeMVar chw
    chr <- if null chrs then newChan else dupChan (head chrs)
    putMVar chw (chr : chrs)
    pure chr

removeChanR chw chr = modifyMVar_ chw $ \ chrs ->
    pure $ delete chr chrs

readChanR chr = readChan chr

writeChanW chw x = do
    chrs <- takeMVar chw
    case chrs of
        ch : _ -> writeChan ch x
        [] -> pure ()
    putMVar chw chrs



emissor ::  (Show a) => Int -> [a] -> ChanW a  ->  IO ()
emissor  id [] chn   = pure ()
emissor  id (x:xs) chn   = do
                temps <- randomRIO (1::Int, 6)
                retardar temps
                writeChanW chn x
                atomicPutStrLn $ "Enviat < " ++ (show id) ++ " > : " ++ show x
                -- loop back
                emissor id xs chn  
{-
emissor id xs chn  = forM_ xs $ \ x -> do
                    temps <- randomRIO (1::Int, 6)
                    retardar temps
                    writeChanW chn x
                    atomicPutStrLn $ "Enviat < " ++ (show id) ++ " > : " ++ show x
-}

receptor :: (Show a) => Int -> Int -> ChanR a -> IO ()
receptor id 0 _ = undefined


{-

                                                    
                      bCastChan                      
                 +----------------+ chanR1           
                 |                |+------+          
                 | +------------> || ---> |receptor0 
emissor0 +------+|/               |+------+          
         | ---> ||                |                  
emissor1 +------+|\               |+------+          
                 | +------------> || ---> |receptor1 
                 |                |+------+          
                 +----------------+ chanR2           
-}


provaChan :: IO ()
provaChan = do
    bCastChan <- newBroadcastChanW
    let dades = "abcde"
    chanR1 <- addChanR bCastChan    
    forkIO $ receptor 0 (2*(length dades)) chanR1    
    chanR2 <- addChanR bCastChan
    forkIO $ receptor 1 (2*(length dades)) chanR2
    retardar 10
    forkIO $ emissor 0 dades bCastChan
    forkIO $ emissor 1 dades bCastChan
    pure ()


---------------------------------------------------------------------------
---------------------------  Servei d'Eco ---------------------------------
---------------------------------------------------------------------------

---------------------------------------------------------------------------
------------------------------  Servidor ----------------------------------
---------------------------------------------------------------------------


servidorEco :: IO ()
servidorEco = do
                let port = 3000 :: PortNumber
                srvrSckt <-  nouServerSocket port
                infoLog $ "Escoltant pel port " <> show port
                bucleAccept srvrSckt tascaEcoServidor

-- accept :: Socket -> IO (Socket, SockAddr) 
bucleAccept :: Socket -> (Socket -> IO a) -> IO a    
bucleAccept serverSocket tasca  = do 
        (socket, adreRemota) <- accept serverSocket 
        infoLog $ "Connexió acceptada desde " <> show adreRemota 
        -- Crear un thread que executi la tascaChat i que en acabar
        -- la tascaChat tanqui el socket                
        forkFinally (tasca socket) 
                    (\_ -> do
                        infoLog $ "Connexió acabada desde " <> show adreRemota
                        tancarSocket socket
                    )
        bucleAccept serverSocket tasca

tascaEcoServidor :: Socket -> IO ()
tascaEcoServidor sc = do
        sendAll sc $ pack "Entra text: "
        miss <- recv sc 1024
        let missString = unpack miss
        infoLog $ "Rebut -> " ++ missString
        sendAll sc miss
        if missString == "/fi"  then do
                                        infoLog $ "Acabant connexio ... " 
                                        pure ()
                                else do
                                        tascaEcoServidor sc




---------------------------------------------------------------------------
--------------------------  Client Eco ------------------------------------
---------------------------------------------------------------------------


clientEco :: IO ()
clientEco = exeClient "127.0.0.1" "3000" tascaEcoClient

exeClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
exeClient host port tasca =  do
    sck <- obrirSocket host port
    tasca sck

tascaEcoClient :: Socket -> IO ()
tascaEcoClient sc = bucle
    where bucle = do
                    queFer <- recv sc 1024
                    putStrLn $ unpack queFer
                    txt <- getLine
                    sendAll sc (pack txt)
                    putStrLn "" 
                    miss <- recv sc 1024
                    let missRebut = unpack miss
                    putStrLn $ "Rebut -> " ++ missRebut
                    if missRebut == "/fi" then do
                                              tancarSocket sc
                                              infoLog "Client finalitzat"
                                              pure ()
                                          else bucle          


---------------------------------------------------------------------------
-----------------  Client General Multithread -----------------------------
---------------------------------------------------------------------------

clientGeneral :: IO ()
clientGeneral = exeClient "127.0.0.1" "3000" tascaClient


-- Espera a llegir del socket i 
-- mostra el que ha llegit per pantalla
tascaXarxa :: Socket -> IO ()
tascaXarxa sc = undefined

-- Espera a llegir del teclat i 
-- envia el que s'ha escrit pel socket
tascaTeclat :: Socket -> IO ()
tascaTeclat sc = undefined



-- executa tascaXarxa i tascaTeclat en Threads diferents
tascaClient :: Socket -> IO ()
tascaClient sc = do
                tascaXarxaId <- forkIO $ tascaXarxa sc
                tascaTeclat sc

---------------------------------------------------------------------------
--------------------------  Servidor Eco Nick -----------------------------
---------------------------------------------------------------------------

-- Dades Usuari

data Usuari = Usuari
        { nick :: Nick
        , socketUsr :: Socket
        }

type Nick = String

servidorEcoNick :: IO ()
servidorEcoNick = do
                let port = 3000 :: PortNumber
                srvrSckt <-  nouServerSocket port
                infoLog $ "Escoltant pel port " <> show port
                bucleAccept srvrSckt tascaEcoServidorNick


tascaEcoServidorNick :: Socket -> IO ()
tascaEcoServidorNick sc = do
        usuari <- obtenirNick sc
        bucleEco usuari 

obtenirNick :: Socket -> IO Usuari
obtenirNick sc = undefined

bucleEco :: Usuari -> IO ()
bucleEco usuari = undefined



---------------------------------------------------------------------------
------------------  Servidor Eco Nick Unic      ---------------------------
---------------------------------------------------------------------------


-- Dades Servidor

data Servidor = Servidor { connectats :: MVar (M.Map Nick Usuari) }

crearServ :: IO Servidor
crearServ = do
    connectats <- newMVar M.empty
    pure $ Servidor connectats 




------------------------  Gestió d'usuaris ------------------------------------


servidorEcoNickUnic :: IO ()
servidorEcoNickUnic = do
                let port = 3000 :: PortNumber
                srvrSckt <-  nouServerSocket port
                infoLog $ "Escoltant pel port " <> show port
                servidor <- crearServ
                bucleAccept srvrSckt (tascaEcoServidorNickUnic servidor)


-- intenta afegir un nou usuari al mapa del servidor
-- La funció té com a resultat una acció que produeix:
--  - Just usuari, si no hi cap usuari amb nick 'nom'
--  - Nothing, si ja hi ha un usuari amb nick 'nom' 
afegirUsuari :: Nick -> Socket -> Servidor -> IO (Maybe Usuari)
afegirUsuari nom socket servidor = undefined



-- treu l'usuari del mapa del servidor
treureUsuari :: Usuari -> Servidor -> IO ()
treureUsuari usuari servidor = undefined


-- demana un nick a l'usuari fins que aquest li
-- envia un nick que no estigui sent utilitzat
obtenirNickUnic :: Servidor -> Socket -> IO Usuari
obtenirNickUnic servidor sc = undefined


-- veure funcio tascaEcoServidorNick 
tascaEcoServidorNickUnic ::  Servidor -> Socket ->IO ()
tascaEcoServidorNickUnic servidor sc  = undefined




---------------------------------------------------------------------------
------------------------------  Utilitats ---------------------------------
---------------------------------------------------------------------------


nouServerSocket :: PortNumber -> IO Socket
nouServerSocket port = do
    sock <- socket AF_INET Stream 0   -- create socket
    setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet port 0)   -- listen on the provided TCP port.
    listen sock 2                     -- set a max of 2 queued connections
    pure sock


obrirSocket :: HostName -> ServiceName -> IO Socket
obrirSocket host port = do
        addr <- determinarAdr
        sock <- openSocket addr
        connect sock (addrAddress addr)
        return sock
  where
    determinarAdr = do
        let configuracio = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just configuracio) (Just host) (Just port)


tancarSocket :: Socket -> IO ()
tancarSocket sc = do
    debugLog "Socket tancat"
    gracefulClose sc 5000

infoLog :: String -> IO ()
infoLog str = do
    putStr "INFO: "
    putStrLn str

debugLog :: String -> IO ()
debugLog str =
    when dEBUG $ do
        putStr "DEBUG: "
        putStrLn str

dEBUG :: Bool
dEBUG = True

