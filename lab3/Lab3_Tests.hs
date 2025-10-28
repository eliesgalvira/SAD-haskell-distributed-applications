import Test.HUnit
import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import System.Exit (exitFailure)
import Lab3_1

-- Custom Counts data type to avoid conflict with HUnit's Counts
data MyCounts = MyCounts { myCases :: Int, myTried :: Int, myErrors :: Int, myFailures :: Int }
  deriving (Eq, Show)

addCounts :: MyCounts -> MyCounts -> MyCounts
addCounts (MyCounts c1 t1 e1 f1) (MyCounts c2 t2 e2 f2) =
  MyCounts (c1 + c2) (t1 + t2) (e1 + e2) (f1 + f2)

-- Extract passes: successfully tried minus failures
myPasses :: MyCounts -> Int
myPasses (MyCounts _ t _ f) = t - f

-- Execute a single case with label
executeCase :: IO () -> String -> IO MyCounts
executeCase act label = do
  e <- try act :: IO (Either SomeException ())
  case e of
    Right _ -> do
      putStrLn $ "PASS: " ++ label
      return $ MyCounts 1 1 0 0
    Left ex -> do
      let msg = show ex
      let prefix = take 15 msg
      let isAssertFail = prefix == "AssertionFailed \""
      let reason = if isAssertFail
                     then init (drop 15 msg)
                     else msg
      let status = if isAssertFail then "FAIL" else "ERROR"
      putStrLn $ status ++ ": " ++ label ++ " (" ++ reason ++ ")"
      if isAssertFail
        then return $ MyCounts 1 1 0 1
        else return $ MyCounts 1 0 1 0

-- Verbose runner: traverses the test tree and executes cases
verboseRun :: Test -> IO MyCounts
verboseRun test = case test of
  TestList ts ->
    foldM (\acc t -> verboseRun t >>= \r -> return (addCounts acc r)) (MyCounts 0 0 0 0) ts
  TestLabel label (TestCase act) ->
    executeCase act label
  TestCase act ->
    executeCase act "Unnamed Case"
  TestLabel label sub ->
    verboseRun sub  -- Recurse, label will be handled in sub if applicable

-- 1.1: Bit equality
testsBit :: Test
testsBit = TestList [
    TestLabel "Bit O == O" (TestCase (assertBool "O == O" (O .==. O))),
    TestLabel "Bit I == I" (TestCase (assertBool "I == I" (I .==. I))),
    TestLabel "Bit O /= I" (TestCase (assertBool "O /= I" (not (O .==. I)))),
    TestLabel "Bit I /= O" (TestCase (assertBool "I /= O" (not (I .==. O))))
  ]

-- 1.2: bitAint and DBit constructors
testsDBit :: Test
testsDBit = TestList [
    TestLabel "bitAint O" (TestCase (assertEqual "bitAint O" 0 (bitAint O))),
    TestLabel "bitAint I" (TestCase (assertEqual "bitAint I" 1 (bitAint I))),

    TestLabel "dbitAint D O O" (TestCase (assertEqual "dbitAint (D O O)" 0 (dbitAint (D O O)))),
    TestLabel "dbitAint D O I" (TestCase (assertEqual "dbitAint (D O I)" 1 (dbitAint (D O I)))),
    TestLabel "dbitAint D I O" (TestCase (assertEqual "dbitAint (D I O)" 2 (dbitAint (D I O)))),
    TestLabel "dbitAint D I I" (TestCase (assertEqual "dbitAint (D I I)" 3 (dbitAint (D I I))))
  ]

-- 1.3: Pattern matching version
testsDBitP :: Test
testsDBitP = TestList [
    TestLabel "dbitAintP D O O" (TestCase (assertEqual "dbitAintP (D O O)" 0 (dbitAintP (D O O)))),
    TestLabel "dbitAintP D O I" (TestCase (assertEqual "dbitAintP (D O I)" 1 (dbitAintP (D O I)))),
    TestLabel "dbitAintP D I O" (TestCase (assertEqual "dbitAintP (D I O)" 2 (dbitAintP (D I O)))),
    TestLabel "dbitAintP D I I" (TestCase (assertEqual "dbitAintP (D I I)" 3 (dbitAintP (D I I))))
  ]

-- 1.4: Nat functions
testsNat :: Test
testsNat = TestList [
    TestLabel "natAint Zero" (TestCase (assertEqual "natAint Zero" 0 (natAint Zero))),
    TestLabel "natAint S Zero" (TestCase (assertEqual "natAint (S Zero)" 1 (natAint (S Zero)))),
    TestLabel "natAint S S Zero" (TestCase (assertEqual "natAint (S (S Zero))" 2 (natAint (S (S Zero))))),

    TestLabel "sumaNat Zero Zero" (TestCase (assertEqual "sumaNat Zero Zero" Zero (sumaNat Zero Zero))),
    TestLabel "sumaNat S Zero Zero" (TestCase (assertEqual "sumaNat (S Zero) Zero" (S Zero) (sumaNat (S Zero) Zero))),
    TestLabel "sumaNat Zero S Zero" (TestCase (assertEqual "sumaNat Zero (S Zero)" (S Zero) (sumaNat Zero (S Zero)))),
    TestLabel "sumaNat S Zero S Zero" (TestCase (assertEqual "sumaNat (S Zero) (S Zero)" (S (S Zero)) (sumaNat (S Zero) (S Zero)))),

    TestLabel "eqNat Zero Zero true" (TestCase (assertBool "eqNat Zero Zero" (eqNat Zero Zero))),
    TestLabel "eqNat S Zero S Zero true" (TestCase (assertBool "eqNat (S Zero) (S Zero)" (eqNat (S Zero) (S Zero)))),
    TestLabel "eqNat Zero S Zero false" (TestCase (assertBool "eqNat Zero (S Zero) false" (not (eqNat Zero (S Zero))))),

    TestLabel "ltNat Zero S Zero true" (TestCase (assertBool "ltNat Zero (S Zero)" (ltNat Zero (S Zero)))),
    TestLabel "ltNat S Zero Zero false" (TestCase (assertBool "ltNat (S Zero) Zero false" (not (ltNat (S Zero) Zero)))),
    TestLabel "ltNat Zero Zero false" (TestCase (assertBool "ltNat Zero Zero false" (not (ltNat Zero Zero)))),

    TestLabel "restaNat Zero Zero" (TestCase (assertEqual "restaNat Zero Zero" Zero (restaNat Zero Zero))),
    TestLabel "restaNat S Zero Zero" (TestCase (assertEqual "restaNat (S Zero) Zero" (S Zero) (restaNat (S Zero) Zero))),
    TestLabel "restaNat S S Zero S Zero" (TestCase (assertEqual "restaNat (S (S Zero)) (S Zero)" (S Zero) (restaNat (S (S Zero)) (S Zero)))),
    TestLabel "restaNat Zero S Zero" (TestCase (assertEqual "restaNat Zero (S Zero)" Zero (restaNat Zero (S Zero))))
  ]

-- 1.4: MInt conversions and sumaMInt (with Int helpers)
testsMIntConv :: Test
testsMIntConv = TestList [
    TestLabel "intAmint 0" (TestCase (assertEqual "intAmint 0" (Pos Zero) (intAmint 0))),
    TestLabel "intAmint 1" (TestCase (assertEqual "intAmint 1" (Pos (S Zero)) (intAmint 1))),
    TestLabel "intAmint -1" (TestCase (assertEqual "intAmint -1" (Neg (S Zero)) (intAmint (-1)))),

    TestLabel "mintAint Pos Zero" (TestCase (assertEqual "mintAint (Pos Zero)" 0 (mintAint (Pos Zero)))),
    TestLabel "mintAint Pos S Zero" (TestCase (assertEqual "mintAint (Pos (S Zero))" 1 (mintAint (Pos (S Zero))))),
    TestLabel "mintAint Neg S Zero" (TestCase (assertEqual "mintAint (Neg (S Zero))" (-1) (mintAint (Neg (S Zero))))),

    TestLabel "roundtrip intAmint mintAint 0" (TestCase (assertEqual "roundtrip 0" 0 (mintAint (intAmint 0)))),
    TestLabel "roundtrip intAmint mintAint 5" (TestCase (assertEqual "roundtrip 5" 5 (mintAint (intAmint 5)))),
    TestLabel "roundtrip intAmint mintAint -3" (TestCase (assertEqual "roundtrip -3" (-3) (mintAint (intAmint (-3)))))
  ]

-- 1.4: sumaMInt (Int-based)
testsSumaMInt :: Test
testsSumaMInt = TestList [
    TestLabel "sumaMInt Pos 1 + Pos 2" (TestCase (assertEqual "Pos 1 + Pos 2" (Pos (S (S (S Zero)))) (sumaMInt (Pos (S Zero)) (Pos (S (S Zero)))))),
    TestLabel "sumaMInt Pos 0 + Pos 3" (TestCase (assertEqual "0 + 3 = 3" (Pos (S (S (S Zero)))) (sumaMInt (Pos Zero) (Pos (S (S (S Zero))))))),

    TestLabel "sumaMInt Neg 1 + Neg 2 = -3" (TestCase (assertEqual "Neg 1 + Neg 2 = -3" (Neg (S (S (S Zero)))) (sumaMInt (Neg (S Zero)) (Neg (S (S Zero)))))),

    TestLabel "sumaMInt 89 + (-89) = 0" (TestCase (assertEqual "sumaMInt 89 + (-89) = 0" (Pos Zero) (sumaMInt (intAmint 89) (intAmint (-89))))),

    TestLabel "sumaMInt 2 + (-1) = 1" (TestCase (assertEqual "2 + (-1) = 1" (Pos (S Zero)) (sumaMInt (Pos (S (S Zero))) (Neg (S Zero))))),
    TestLabel "sumaMInt 1 + (-2) = -1" (TestCase (assertEqual "1 + (-2) = -1" (Neg (S Zero)) (sumaMInt (Pos (S Zero)) (Neg (S (S Zero)))))),

    TestLabel "sumaMInt -1 + 2 = 1" (TestCase (assertEqual "-1 + 2 = 1" (Pos (S Zero)) (sumaMInt (Neg (S Zero)) (Pos (S (S Zero)))))),
    TestLabel "sumaMInt -2 + 1 = -1" (TestCase (assertEqual "-2 + 1 = -1" (Neg (S Zero)) (sumaMInt (Neg (S (S Zero))) (Pos (S Zero)))))
  ]

-- 1.4: sumaMIntD (Pure Nat version)
testsSumaMIntD :: Test
testsSumaMIntD = TestList [
    TestLabel "sumaMIntD Pos Zero + Pos S Zero" (TestCase (assertEqual "Pos Zero + anything = anything" (Pos (S Zero)) (sumaMIntD (Pos Zero) (Pos (S Zero))))),
    TestLabel "sumaMIntD Neg S Zero + Pos Zero" (TestCase (assertEqual "anything + Pos Zero = anything" (Neg (S Zero)) (sumaMIntD (Neg (S Zero)) (Pos Zero)))),

    TestLabel "sumaMIntD Pos 1 + Pos 2 = 3" (TestCase (assertEqual "Pos 1 + Pos 2 = 3" (Pos (S (S (S Zero)))) (sumaMIntD (Pos (S Zero)) (Pos (S (S Zero)))))),

    TestLabel "sumaMIntD Neg 1 + Neg 2 = -3" (TestCase (assertEqual "Neg 1 + Neg 2 = -3" (Neg (S (S (S Zero)))) (sumaMIntD (Neg (S Zero)) (Neg (S (S Zero)))))),

    TestLabel "sumaMIntD Pos 2 + Neg 1 = 1" (TestCase (assertEqual "Pos 2 + Neg 1 = 1 (m > n)" (Pos (S Zero)) (sumaMIntD (Pos (S (S Zero))) (Neg (S Zero))))),
    TestLabel "sumaMIntD Pos 1 + Neg 2 = -1" (TestCase (assertEqual "Pos 1 + Neg 2 = -1 (m < n)" (Neg (S Zero)) (sumaMIntD (Pos (S Zero)) (Neg (S (S Zero)))))),
    TestLabel "sumaMIntD Pos 1 + Neg 1 = 0" (TestCase (assertEqual "Pos 1 + Neg 1 = 0 (equal)" (Pos Zero) (sumaMIntD (Pos (S Zero)) (Neg (S Zero))))),

    TestLabel "sumaMIntD Neg 1 + Pos 2 = 1" (TestCase (assertEqual "Neg 1 + Pos 2 = 1" (Pos (S Zero)) (sumaMIntD (Neg (S Zero)) (Pos (S (S Zero)))))),
    TestLabel "sumaMIntD Neg 2 + Pos 1 = -1" (TestCase (assertEqual "Neg 2 + Pos 1 = -1" (Neg (S Zero)) (sumaMIntD (Neg (S (S Zero))) (Pos (S Zero))))),
    TestLabel "sumaMIntD Neg 1 + Pos 1 = 0" (TestCase (assertEqual "Neg 1 + Pos 1 = 0" (Pos Zero) (sumaMIntD (Neg (S Zero)) (Pos (S Zero))))),

    TestLabel "sumaMIntD Pos S S Zero + Neg S S Zero = 0" (TestCase (assertEqual "Pos (S (S Zero)) + Neg (S (S Zero)) = 0" (Pos Zero) (sumaMIntD (Pos (S (S Zero))) (Neg (S (S Zero))))))
  ]

-- Main suite
mainSuite :: Test
mainSuite = TestList [testsBit, testsDBit, testsDBitP, testsNat, testsMIntConv, testsSumaMInt, testsSumaMIntD]

-- For Cabal batch/GHCi: Verbose run + report
main :: IO ()
main = do
  counts <- verboseRun mainSuite
  putStrLn ""
  putStrLn "Final Report:"
  putStrLn $ "Total cases: " ++ show (myCases counts)
  putStrLn $ "Passes: " ++ show (myPasses counts)
  putStrLn $ "Fails: " ++ show (myFailures counts)
  putStrLn $ "Errors: " ++ show (myErrors counts)
  if myFailures counts + myErrors counts == 0
    then putStrLn "All tests passed!"
    else exitFailure
