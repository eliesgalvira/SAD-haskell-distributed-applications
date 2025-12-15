module Lab7 where

import Control.Monad (ap)
import System.Random (randomRIO)
import Data.List (nub)
import Data.Char (isDigit, digitToInt)

-- ============================================================================
-- Secció 1: El monad EstatIO
-- ============================================================================

-- EstatIO combina el monad State amb el monad IO
newtype EstatIO s a = EstatIO { execEstatIO :: s -> IO (s, a) }

instance Functor (EstatIO s) where
    fmap f eiox = EstatIO $ \s -> do -- monad IO
        (estat', res) <- execEstatIO eiox s
        pure (estat', f res)

instance Applicative (EstatIO s) where
    pure a = EstatIO $ \s -> pure (s, a)
    (<*>) = ap

instance Monad (EstatIO s) where
    return = pure
    -- Bind: executa la primera acció, passa el nou estat a la continuació
    eio >>= k = EstatIO $ \s -> do
        (estat', a) <- execEstatIO eio s
        execEstatIO (k a) estat'

-- Obtenir l'estat actual
obtenirEstatIO :: EstatIO e e
obtenirEstatIO = EstatIO $ \s -> pure (s, s)

-- Canviar l'estat
canviarEstatIO :: e -> EstatIO e ()
canviarEstatIO est = EstatIO $ \_ -> pure (est, ())

-- Utilitzar accions IO en el monad EstatIO
-- Construeix una acció EstatIO que quan s'executi
-- produirà el valor que produeix l'acció IO a l'entrada
pujarIO :: IO a -> EstatIO b a
pujarIO io = EstatIO $ \estat -> do -- monad IO
    x <- io
    pure (estat, x)

-- ============================================================================
-- Secció 1.1: Interacció amb l'usuari - Exercici EstatProva
-- ============================================================================

-- Estat de prova: llista d'entrades i número d'entrades restants
data EstatProva = EP { entrades :: [String], numEntrades :: Int }
    deriving Show

-- Demana línies de text a l'usuari i les guarda a l'estat
provaEstatIO :: EstatIO EstatProva ()
provaEstatIO = do
    estat <- obtenirEstatIO
    if numEntrades estat == 0
        then do
            pujarIO $ putStrLn "Entrades guardades:"
            pujarIO $ mapM_ putStrLn (reverse $ entrades estat)
        else do
            pujarIO $ putStr "Entra una línia: "
            linia <- pujarIO getLine
            canviarEstatIO $ EP (linia : entrades estat) (numEntrades estat - 1)
            provaEstatIO

-- Executar la prova amb un estat inicial de 3 entrades
executarProva :: IO (EstatProva, ())
executarProva = execEstatIO provaEstatIO (EP [] 3)

-- ============================================================================
-- Secció 2: Joc Bons i Dolents
-- ============================================================================

-- Tipus per representar una jugada (4 dígits)
type Jugada = [Int]

-- Estat del joc
data EstatJoc = EJ
    { secret       :: Jugada      -- El número secret a endevinar
    , jugades      :: [Jugada]    -- Llista de jugades fetes
    , intentsRestants :: Int      -- Intents que queden
    } deriving Show

-- Nombre inicial d'intents
maxIntents :: Int
maxIntents = 10

-- Genera un número aleatori de 4 dígits diferents
generarSecret :: IO Jugada
generarSecret = do
    let digits = [0..9]
    d1 <- randomRIO (0, 9)
    d2 <- pickDifferent [d1]
    d3 <- pickDifferent [d1, d2]
    d4 <- pickDifferent [d1, d2, d3]
    return [d1, d2, d3, d4]
  where
    pickDifferent :: [Int] -> IO Int
    pickDifferent used = do
        d <- randomRIO (0, 9)
        if d `elem` used then pickDifferent used else return d

-- Valida una jugada: 4 dígits tots diferents
validarJugada :: String -> Maybe Jugada
validarJugada str
    | length str /= 4 = Nothing
    | not (all isDigit str) = Nothing
    | let digits = map digitToInt str in nub digits /= digits = Nothing
    | otherwise = Just (map digitToInt str)

-- Calcula el número de Bons (posició correcta) i Dolents (dígit correcte, posició incorrecta)
calcularBonsDolents :: Jugada -> Jugada -> (Int, Int)
calcularBonsDolents secretJ jugada = (bons, dolents)
  where
    -- Bons: dígits en la mateixa posició
    bons = length $ filter (uncurry (==)) $ zip secretJ jugada
    -- Dígits correctes (independentment de la posició)
    correctes = length $ filter (`elem` secretJ) jugada
    -- Dolents: correctes que no estan en la posició correcta
    dolents = correctes - bons

-- Mostra les jugades prèvies
mostrarJugades :: [Jugada] -> EstatIO EstatJoc ()
mostrarJugades js = do
    pujarIO $ putStrLn ""
    pujarIO $ putStrLn "Jugades previes:"
    pujarIO $ mapM_ print js

-- Demana una jugada vàlida a l'usuari
demanarJugada :: EstatIO EstatJoc Jugada
demanarJugada = do
    pujarIO $ putStr "Entra jugada : "
    input <- pujarIO getLine
    case validarJugada input of
        Nothing -> do
            pujarIO $ putStrLn "Jugada no valida"
            demanarJugada
        Just j -> return j

-- Bucle principal del joc
bucleJoc :: EstatIO EstatJoc ()
bucleJoc = do
    jugada <- demanarJugada
    estat <- obtenirEstatIO
    let (bons, dolents) = calcularBonsDolents (secret estat) jugada
    let novesJugades = jugada : jugades estat
    let novsIntents = intentsRestants estat - 1
    
    -- Actualitzar estat
    canviarEstatIO $ estat { jugades = novesJugades, intentsRestants = novsIntents }
    
    if bons == 4
        then do
            -- Guanyat!
            pujarIO $ putStrLn ""
            pujarIO $ putStrLn "Has Guanyat!"
            mostrarJugades novesJugades
        else if novsIntents == 0
            then do
                -- Perdut!
                pujarIO $ putStrLn ""
                pujarIO $ putStr "El secret era : "
                pujarIO $ print (secret estat)
                pujarIO $ putStrLn ""
                pujarIO $ putStrLn "Has Perdut!"
                mostrarJugades novesJugades
            else do
                -- Continuar jugant
                pujarIO $ putStrLn ""
                pujarIO $ putStrLn $ "Bons : " ++ show bons ++ " -- Dolents : " ++ show dolents ++ " -- Queden " ++ show novsIntents ++ " intents"
                pujarIO $ putStrLn ""
                pujarIO $ putStrLn "Seguir Jugant"
                mostrarJugades novesJugades
                pujarIO $ putStrLn ""
                bucleJoc

-- Funció principal per iniciar el joc
jugar :: IO ()
jugar = do
    secretNum <- generarSecret
    let estatInicial = EJ { secret = secretNum, jugades = [], intentsRestants = maxIntents }
    _ <- execEstatIO bucleJoc estatInicial
    return ()

-- ============================================================================
-- Main per executar directament
-- ============================================================================

main :: IO ()
main = jugar

