module Codec.TCPSegment where

import Data.Char (chr)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

-- Funcions útils (Secció 5)

-- Converteix un enter positiu a la seva representació binària
-- [b_0, b_1, ... , b_{n-2}, b_{n-1}]
-- on b_{n-1} és el bit menys significatiu
intAbit :: Int -> [Int]
intAbit 0 = []
intAbit n = let r = n `mod` 2
            in (intAbit $ n `div` 2) ++ [r]

-- Si la mida de la llista d'entrada és:
-- - més gran de 8 es queda amb els 8 bits menys significatius
-- - altrament omple amb zeros els bits més significatius fins arribar a 8
mida8 :: [Int] -> [Int]
mida8 xs
    | length xs > 8 = drop ((length xs) - 8) xs
    | length xs == 8 = xs
    | otherwise = mida8 (0:xs)

-- Converteix un enter positiu menor que 255 a la seva representació binària de 8 bits
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7]
-- on b_7 és el bit menys significatiu
intA8bit :: Int -> [Int]
intA8bit = mida8 . intAbit

-- Converteix la representació binària d'un enter positiu
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7]
-- on b_7 és el bit menys significatiu
-- a l'enter
vuitBitaInt :: [Int] -> Int
vuitBitaInt = foldl (\a b -> 2*a + b) 0

-- Converteix un caràcter en el valor binari del seu codi ASCII
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7] on b_7 és el bit menys significatiu
-- fromEnum dona el valor ASCII decimal del caràcter
charA8bit :: Char -> [Int]
charA8bit = intA8bit . fromEnum

-- Converteix un valor de codi ASCII expressat com
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7]
-- on b_7 és el bit menys significatiu al seu caràcter corresponent.
bit8Achar :: [Int] -> Char
bit8Achar = chr . vuitBitaInt

-- Converteix una llista de caràcters a una llista de bits codificant cada caràcter
-- amb la representació binària del seu codi ASCII
stringA8bit :: [Char] -> [Int]
stringA8bit = concat . map charA8bit

-- Separa una llista en vàries llistes de mida 8. Si la longitud no és múltiple de 8
-- afegeix zeros a l'última llista fins a arribar a mida 8
parts8bits :: [Int] -> [[Int]]
parts8bits xs
    | length xs <= 8 = [mida8 xs]
    | otherwise = (take 8 xs):(parts8bits (drop 8 xs))

-- Donades dues llistes d'enters d'igual longitud, interpreta cada element com un bit,
-- i fa la suma bit a bit.
sumBitaBit :: [Int] -> [Int] -> [Int]
sumBitaBit [] _ = []
sumBitaBit (x:xs) (y:ys) = ((x+y) `mod` 2):(sumBitaBit xs ys)

-- Donada una llista de llistes d'enters d'igual longitud, interpreta cada element
-- com un bit, i fa una suma binària per posicions.
sumarBitaBit :: [[Int]] -> [Int]
sumarBitaBit [] = []
sumarBitaBit xs = foldr (\ys zs -> sumBitaBit ys zs) (replicate (length (head xs)) 0) xs

-- Tipus de dades (Secció 2)

-- 1. NumSeq: Nou nom per Int
type NumSeq = Int

-- 2. CheckSum: Llista d'enters (bits 0/1, invariant: len=8, elements 0/1)
type CheckSum = [Int]

-- 3. TipusSeg: Constructors PSH, ACK, SYN, FIN (en aquest ordre)
data TipusSeg = PSH | ACK | SYN | FIN
  deriving (Show, Eq, Enum)

-- 4. Capçalera: TipusSeg, NumSeq, CheckSum
data Capcalera = Cap TipusSeg NumSeq CheckSum
  deriving (Show, Eq)

-- 5. Dades: Llista d'enters (utilitzar data)
data Dades = D [Int]
  deriving (Show)

-- 6. TCPSegment: Capçalera i Dades
data TCPSegment = Segment Capcalera Dades
  deriving (Show)

-- Instàncies per TCPSegment (Show, Eq, Ord)
instance Eq TCPSegment where
  (Segment (Cap t1 n1 c1) (D d1)) == (Segment (Cap t2 n2 c2) (D d2)) =
    t1 == t2 && n1 == n2 && c1 == c2 && d1 == d2

instance Ord TCPSegment where
  (Segment (Cap _ n1 _) _) <= (Segment (Cap _ n2 _) _) = n1 <= n2

-- Helper per validar bits: 0/1
validarBits :: [Int] -> Bool
validarBits bs = all (\b -> b == 0 || b == 1) bs

-- Decodificació de funcions (Secció 4.2)

-- Helper: 8 bits a TipusSeg (via Enum) - retorna Nothing si invariant violat
bitsToTipusSeg :: [Int] -> Maybe TipusSeg
bitsToTipusSeg bs = do
  guard (length bs == 8 && validarBits bs)
  let val = vuitBitaInt bs
  case val of
    0 -> Just PSH
    1 -> Just ACK
    2 -> Just SYN
    3 -> Just FIN
    _ -> Nothing  -- Invariant violat: TipusSeg no és 0-3

-- Helper: 8 bits a NumSeq (Int [0..255]) - retorna Nothing si invariant violat
bitsToNumSeq :: [Int] -> Maybe NumSeq
bitsToNumSeq bs = do
  guard (length bs == 8 && validarBits bs)
  let val = vuitBitaInt bs
  guard (val >= 0 && val <= 255)  -- Invariant: NumSeq dins [0..255]
  return val

-- Helper: 8 bits a CheckSum - retorna Nothing si invariant violat
bitsToCheckSum :: [Int] -> Maybe CheckSum
bitsToCheckSum bs = do
  guard (length bs == 8 && validarBits bs)  -- Invariant: CheckSum té 8 bits vàlids
  return bs

-- Funció principal: bits a TCPSegment - retorna Nothing si invariant violat
bitsToSegment :: [Int] -> Maybe TCPSegment
bitsToSegment bits = do
  guard (length bits >= 24)  -- Invariant: segment té almenys 24 bits (header)
  let headerBits = take 24 bits
      dataBits = drop 24 bits
      tipusBits = take 8 headerBits
      numBits = take 8 (drop 8 headerBits)
      checkBits = take 8 (drop 16 headerBits)
  tipus <- bitsToTipusSeg tipusBits
  numSeq <- bitsToNumSeq numBits
  checkSum <- bitsToCheckSum checkBits
  guard (validarBits dataBits)  -- Invariant: dades són bits vàlids
  return $ Segment (Cap tipus numSeq checkSum) (D dataBits)

-- Detecció d'errors (Secció 4.4)

-- Helper: Calcular checksum de Dades - retorna Nothing si invariant violat
calcularCheckSum :: Dades -> Maybe CheckSum
calcularCheckSum (D ds) = do
  guard (length ds `mod` 8 == 0)  -- Invariant: dades múltiple de 8 bits
  guard (validarBits ds)  -- Invariant: bits vàlids (0/1)
  let chunks = parts8bits ds
  if null chunks 
  then return (replicate 8 0)
  else return (mida8 (sumarBitaBit chunks))

-- Funció: Detecta error si checksum rebut != calculat - retorna Nothing si invariant violat
ambError :: TCPSegment -> Maybe Bool
ambError (Segment (Cap _ _ checkRebut) dades) = do
  checkCalculat <- calcularCheckSum dades
  return (checkRebut /= checkCalculat)

-- Processament de pipeline (Secció 4)

-- Helper: Dades a String (chunks de 8 bits a Char)
dadesToString :: Dades -> String
dadesToString (D ds) = 
  let chunks = parts8bits ds
      chars = map bit8Achar chunks
  in chars

-- Processament: Llista bits a llista segments sense error
segmentsSenseError :: [[Int]] -> [TCPSegment]
segmentsSenseError bitSegments = 
  let segments = mapMaybe bitsToSegment bitSegments
      teError seg = case ambError seg of
        Just True -> True
        Just False -> False
        Nothing -> True  -- Si no es pot calcular checksum, considerar error
  in filter (not . teError) segments

-- Filtrar PSH
segmentsPSH :: [TCPSegment] -> [TCPSegment]
segmentsPSH segs = filter (\(Segment (Cap t _ _) _) -> t == PSH) segs

-- Eliminar duplicats i ordenar per NumSeq (usant Ord/Eq)
segmentsUnicsOrdenats :: [TCPSegment] -> [TCPSegment]
segmentsUnicsOrdenats segs = 
  let unics = nub segs
  in sort unics

-- Decodificar missatge final
decodificarMissatge :: [[Int]] -> String
decodificarMissatge bitSegments = 
  let segsSenseError = segmentsSenseError bitSegments
      pshSegs = segmentsPSH segsSenseError
      unicsOrdenats = segmentsUnicsOrdenats pshSegs
      dadesConcat = concatMap (\(Segment _ d) -> dadesToString d) unicsOrdenats
  in dadesConcat

-- Helpers per a tests

-- Getter per a tests
getTipusSeg :: TCPSegment -> TipusSeg
getTipusSeg (Segment (Cap t _ _) _) = t

getNumSeq :: TCPSegment -> NumSeq
getNumSeq (Segment (Cap _ n _) _) = n

getDadesBits :: Dades -> [Int]
getDadesBits (D ds) = ds

-- Helper: Int (0..2^24-1) a [Int] de exactament 24 bits - retorna Nothing si invariant violat
intTo24Bits :: Int -> Maybe [Int]
intTo24Bits i 
  | i < 0 || i > (2^24 - 1) = Nothing  -- Invariant violat: fora de rang
  | otherwise = 
    let bits = intAbit i
    in Just (replicate (24 - length bits) 0 ++ bits)
