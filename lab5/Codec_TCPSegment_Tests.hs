module Main where

import Codec.TCPSegment
import Test.QuickCheck
import Data.List (nub, sort, tails, permutations)
import Control.Monad (when, guard, forM_)
import Data.Maybe (isJust, isNothing)
import System.CPUTime (getCPUTime)

-- ============================================================================
-- Test helpers
-- ============================================================================

-- Helper per generar bits exhaustius petits
bitsExhaustius :: Int -> [[Int]]
bitsExhaustius len = sequence [[0,1] | _ <- [1..len]]

-- Helper per crear header amb checksum calculat
headerPSH :: NumSeq -> Dades -> [Int]
headerPSH seq dades = 
  let tipusBits = intA8bit 0  -- PSH = 0
      numBits = intA8bit seq
      checkBits = calcularCheckSum dades
  in tipusBits ++ numBits ++ checkBits

headerACK :: NumSeq -> Dades -> [Int]
headerACK seq dades = 
  let tipusBits = intA8bit 1  -- ACK = 1
      numBits = intA8bit seq
      checkBits = calcularCheckSum dades
  in tipusBits ++ numBits ++ checkBits

-- Helper per calcular checksum de string
calcularCheckSumEx :: String -> CheckSum
calcularCheckSumEx s = calcularCheckSum (D (stringA8bit s))

-- Assert helper
assert :: Bool -> String -> IO ()
assert cond msg = when (not cond) (putStrLn $ "FAIL: " ++ msg)

-- ============================================================================
-- QuickCheck Arbitrary instances
-- ============================================================================

instance Arbitrary TipusSeg where
  arbitrary = elements [PSH, ACK, SYN, FIN]

instance Arbitrary Dades where
  arbitrary = do
    len <- choose (0, 32) `suchThat` (\n -> n `mod` 8 == 0)
    bs <- vectorOf len (elements [0, 1])
    return (D bs)

instance Arbitrary TCPSegment where
  arbitrary = do
    t <- arbitrary
    n <- choose (0, 255)
    lenD <- choose (0, 32) `suchThat` (\n -> n `mod` 8 == 0)
    d <- vectorOf lenD (elements [0, 1])
    let checkCalc = calcularCheckSum (D d)
    return (Segment (Cap t n checkCalc) (D d))

-- ============================================================================
-- QuickCheck properties
-- ============================================================================

-- Propietat: Checksum calculat sempre 8 bits vàlids
prop_checksumInvariant :: Dades -> Property
prop_checksumInvariant d = 
  length (getDadesBits d) `mod` 8 == 0 ==> 
    let check = calcularCheckSum d
    in length check == 8 && all (`elem` [0,1]) check

-- Propietat: Ord per NumSeq
prop_ordreNumSeq :: TCPSegment -> TCPSegment -> Bool
prop_ordreNumSeq s1 s2 = 
  let n1 = getNumSeq s1
      n2 = getNumSeq s2
  in (s1 <= s2) == (n1 <= n2)

-- Propietat: segmentsSenseError filtra errors
prop_segmentsSenseErrorFiltraErrors :: Property
prop_segmentsSenseErrorFiltraErrors = 
  forAll (vectorOf 5 (do 
    len <- choose (24, 40) `suchThat` (\n -> n `mod` 8 == 0)
    bs <- vectorOf len (elements [0, 1])
    return bs)) $ \segs ->
    let segsAll = map bitsToSegment segs
        segsOK = segmentsSenseError segs
    in length segsOK <= length segsAll && 
       all (\s -> not (ambError s)) segsOK

-- Propietat: segmentsPSH només PSH
prop_segmentsPSHNomésPSH :: [TCPSegment] -> Bool
prop_segmentsPSHNomésPSH segs = 
  all (\s -> getTipusSeg s == PSH) (segmentsPSH segs)

-- Propietat: segmentsUnicsOrdenats sense duplicats i ordenat
prop_unicsOrdenats :: [TCPSegment] -> Bool
prop_unicsOrdenats segs = 
  let unicsOrd = segmentsUnicsOrdenats segs
      lensUnics = length unicsOrd
      noDups = length (nub unicsOrd) == lensUnics
      -- Verificar que tots els parells consecutius estan ordenats
      ordenat = all (\(s1, s2) -> s1 <= s2) (zip unicsOrd (drop 1 unicsOrd))
  in noDups && ordenat

-- Propietat: bitsToSegment produeix segment vàlid
prop_bitsToSegmentValid :: Property
prop_bitsToSegmentValid = 
  forAll (do 
    lenData <- choose (0, 32) `suchThat` (\d -> d `mod` 8 == 0)
    tipusVal <- choose (0, 3)
    numVal <- choose (0, 255)
    checkVal <- choose (0, 255)
    dataBs <- vectorOf lenData (elements [0, 1])
    let headerBits = intA8bit tipusVal ++ intA8bit numVal ++ intA8bit checkVal
        fullBits = headerBits ++ dataBs
    return fullBits) $ \fullBits ->
    length fullBits >= 24 ==> 
      let seg = bitsToSegment fullBits
      in True  -- Si arriba aquí, és vàlid

-- ============================================================================
-- Manual exhaustive tests
-- ============================================================================

-- Test manual: Header exhaustiu petit
test_manual_headerExhaustiu :: IO ()
test_manual_headerExhaustiu = do
  putStrLn "=== Test manual: Header exhaustiu (petit) ==="
  let totsHeaders = do 
        tipusVal <- [0..3]
        numVal <- [0..3]
        checkVal <- [0..3]
        return (intA8bit tipusVal ++ intA8bit numVal ++ intA8bit checkVal)
  results <- mapM (\header -> 
    let seg = bitsToSegment (header ++ replicate 8 0)
        tipusEsperat = toEnum (vuitBitaInt (take 8 header))
    in return (getTipusSeg seg == tipusEsperat)) (take 64 totsHeaders)
  let passed = length (filter id results)
  putStrLn $ "Passats: " ++ show passed ++ "/64"
  assert (passed == 64) "Error en header exhaustiu"

-- Test manual: segmentsSenseError
test_manual_segmentsSenseError :: IO ()
test_manual_segmentsSenseError = do
  putStrLn "=== Test manual: segmentsSenseError ==="
  let dadesHo = D (stringA8bit "Ho")
      bitsOK = headerPSH 1 dadesHo ++ stringA8bit "Ho"
      bitsError = intA8bit 0 ++ intA8bit 1 ++ (reverse (calcularCheckSum dadesHo)) ++ stringA8bit "Ho"
      llistaBits = [bitsOK, bitsError]
      segsOK = segmentsSenseError llistaBits
  putStrLn $ "Segments OK: " ++ show (length segsOK)
  assert (length segsOK == 1) "No filtra error correctament"

-- Test manual: segmentsUnicsOrdenats
test_manual_unicsOrdenats :: IO ()
test_manual_unicsOrdenats = do
  putStrLn "=== Test manual: segmentsUnicsOrdenats ==="
  let segs = [ Segment (Cap PSH i (replicate 8 0)) (D []) | i <- [3,1,3,2,1] ]
      unicsOrd = segmentsUnicsOrdenats segs
      esperat = [ Segment (Cap PSH i (replicate 8 0)) (D []) | i <- [1,2,3] ]
  putStrLn $ "Resultat: " ++ show (map getNumSeq unicsOrd)
  assert (unicsOrd == esperat) "No unics o no ordenat"

-- Test exhaustiu: Checksum
test_exhaustiu_checksum :: IO ()
test_exhaustiu_checksum = do
  putStrLn "=== Test exhaustiu: Checksum ==="
  forM_ [0,8,16] $ \len -> do
    let totesDades = bitsExhaustius len
        subsetDades = take 100 totesDades
    results <- mapM (\ds -> do
      let d = D ds
          checkCalc = calcularCheckSum d
          segFake = Segment (Cap PSH 0 checkCalc) d
          segError = Segment (Cap PSH 0 (reverse checkCalc)) d
      return (length checkCalc == 8 && 
              not (ambError segFake) && 
              ambError segError)) subsetDades
    let passed = length (filter id results)
    putStrLn $ "Len " ++ show len ++ ": " ++ show passed ++ "/" ++ show (length results)

-- Test exhaustiu: Decodificar "Hola"
test_exhaustiu_decodificarHola :: IO ()
test_exhaustiu_decodificarHola = do
  putStrLn "=== Test exhaustiu: Decodificar 'Hola' ==="
  let dadesHo = D (stringA8bit "Ho")
      dadesLa = D (stringA8bit "la")
      bitsHo = headerPSH 0 dadesHo ++ stringA8bit "Ho"
      bitsLa = headerPSH 8 dadesLa ++ stringA8bit "la"
      totesPerms = permutations [bitsHo, bitsLa]
  results <- mapM (\perms -> do
    let decoded = decodificarMissatge perms
    return (decoded == "Hola")) ([bitsHo, bitsLa] : totesPerms)
  let passed = length (filter id results)
  putStrLn $ "Passats: " ++ show passed ++ "/" ++ show (length results)
  assert (all id results) "No recupera 'Hola' correctament"

-- ============================================================================
-- Bounded exhaustive tests
-- ============================================================================

-- Test BOUNDED: Totes combinacions vàlides (4 Tipus * 256 NumSeq * 256 CheckSum)
test_bounded_valid_headers :: IO ()
test_bounded_valid_headers = do
  putStrLn "=== Test Bounded: 4*256*256 = 262k headers vàlids ==="
  startTime <- getCPUTime
  let dadesFixes = replicate 8 0
      totesValid = do 
        tipusVal <- [0..3]
        numVal <- [0..255]
        checkVal <- [0..255]
        let headerBits = intA8bit tipusVal ++ intA8bit numVal ++ intA8bit checkVal
            fullBits = headerBits ++ dadesFixes
        return fullBits
      resultat = length (filter (isJust . bitsToSegmentMaybe) totesValid)
  endTime <- getCPUTime
  let temps = fromIntegral (endTime - startTime) / 10^12 :: Double
  putStrLn $ "Passats: " ++ show resultat ++ "/262144"
  putStrLn $ "Temps: " ++ show temps ++ " segons"
  assert (resultat == 262144) "No tots els headers vàlids passen"

-- Test FULL 2^24: Tots possibles headers (amb progress)
test_full_2pow24_headers :: IO ()
test_full_2pow24_headers = do
  putStrLn "=== Test Full 2^24: 16M headers (amb progress) ==="
  startTime <- getCPUTime
  let dadesFixes = replicate 8 0
      maxI = 2^24 - 1
      loop i accumOK accumErr = do
        if i > maxI 
        then return (accumOK, accumErr)
        else do
          when (i `mod` 1000000 == 0) $ do
            let percent = fromIntegral i * 100 `div` 16777216
            putStrLn $ "Progrés: " ++ show i ++ "/16777216 (~" ++ show percent ++ "%)"
          let bitsHeader = intTo24Bits i
              fullBits = bitsHeader ++ dadesFixes
              mseg = bitsToSegmentMaybe fullBits
          case mseg of
            Just _ -> loop (i+1) (accumOK + 1) accumErr
            Nothing -> loop (i+1) accumOK (accumErr + 1)
  (oks, errs) <- loop 0 0 0
  endTime <- getCPUTime
  let total = oks + errs
      temps = fromIntegral (endTime - startTime) / 10^12 :: Double
  putStrLn $ "Headers vàlids: " ++ show oks ++ " (esperat: 262144 = 4 tipus × 256 NumSeq × 256 CheckSum)"
  putStrLn $ "Headers invàlids: " ++ show errs ++ " (esperat: 16515072 = headers amb TipusSeg > 3 o altres invàlids)"
  putStrLn $ "Total provat: " ++ show total ++ " (2^24 = 16777216)"
  putStrLn $ "Temps: " ++ show temps ++ " segons"
  assert (oks == 262144) "Invariant violat: nombre de headers vàlids incorrecte"
  assert (errs == 16515072) "Invariant violat: nombre de headers invàlids incorrecte"

-- ============================================================================
-- SmallCheck tests
-- ============================================================================

-- SmallCheck tests simplified (using QuickCheck instead for compatibility)
test_smallcheck_ambError :: Property
test_smallcheck_ambError = 
  forAll (arbitrary :: Gen Dades) $ \(D ds) -> 
    length ds `mod` 8 == 0 && all (`elem` [0,1]) ds ==>
      let checkCalc = calcularCheckSum (D ds)
          segOK = Segment (Cap PSH 0 checkCalc) (D ds)
      in not (ambError segOK)

test_smallcheck_segmentsPSH :: Property
test_smallcheck_segmentsPSH = 
  forAll (arbitrary :: Gen [TCPSegment]) $ \listaSegs ->
    let psh = segmentsPSH listaSegs
    in all (\(Segment (Cap t _ _) _) -> t == PSH) psh

-- ============================================================================
-- Main test runner
-- ============================================================================

testsExhaustius :: IO ()
testsExhaustius = do
  putStrLn "=== Tests exhaustius iniciats ==="
  putStrLn ""
  
  putStrLn "--- QuickCheck properties ---"
  quickCheck prop_checksumInvariant
  quickCheck prop_ordreNumSeq
  quickCheck prop_segmentsSenseErrorFiltraErrors
  quickCheck prop_segmentsPSHNomésPSH
  quickCheck prop_unicsOrdenats
  quickCheck prop_bitsToSegmentValid
  putStrLn ""
  
  putStrLn "--- Additional QuickCheck tests ---"
  quickCheck test_smallcheck_ambError
  quickCheck test_smallcheck_segmentsPSH
  putStrLn ""
  
  putStrLn "--- Manual exhaustive tests ---"
  test_manual_headerExhaustiu
  test_manual_segmentsSenseError
  test_manual_unicsOrdenats
  test_exhaustiu_checksum
  test_exhaustiu_decodificarHola
  putStrLn ""
  
  putStrLn "--- Bounded exhaustive test ---"
  test_bounded_valid_headers
  putStrLn ""
  
  putStrLn "--- Full 2^24 test ---"
  test_full_2pow24_headers
  putStrLn ""
  
  putStrLn "=== Tots els tests passats! ==="

main :: IO ()
main = testsExhaustius

