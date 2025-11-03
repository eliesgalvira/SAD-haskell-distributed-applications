import Test.HUnit
import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import System.Exit (exitFailure)
import Data.Monoid (Sum(..))
import Data.Foldable (toList)
import Lab4_1
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

-- Tests per Iterador
testsIterador :: Test
testsIterador = TestList [
    TestLabel "ele retorna primer element" 
        (TestCase (assertEqual "ele (L 1 (L 2 B))" 1 (ele (L 1 (L 2 B))))),
    TestLabel "seg elimina primer element" 
        (TestCase (assertEqual "seg (L 1 (L 2 B))" (L 2 B) (seg (L 1 (L 2 B))))),
    TestLabel "hasnext B és False" 
        (TestCase (assertBool "hasnext B" (not (hasnext (B :: Llista Int))))),
    TestLabel "hasnext (L 1 B) és True" 
        (TestCase (assertBool "hasnext (L 1 B)" (hasnext (L 1 B))))
  ]

-- Tests per sumElem
testsSumElem :: Test
testsSumElem = TestList [
    TestLabel "sumElem llista buida" 
        (TestCase (assertEqual "sumElem B" 0 (sumElem (B :: Llista Int)))),
    TestLabel "sumElem [1,2,3]" 
        (TestCase (assertEqual "sumElem [1,2,3]" 6 (sumElem (L 1 (L 2 (L 3 B)))))),
    TestLabel "sumElem [5]" 
        (TestCase (assertEqual "sumElem [5]" 5 (sumElem (L 5 B))))
  ]

-- Tests per Ord Nat
testsOrdNat :: Test
testsOrdNat = TestList [
    TestLabel "Zero < S Zero" 
        (TestCase (assertBool "Zero < S Zero" (Zero < S Zero))),
    TestLabel "S Zero > Zero" 
        (TestCase (assertBool "S Zero > Zero" ((S Zero) > Zero))),
    TestLabel "S (S Zero) < S (S (S Zero))" 
        (TestCase (assertBool "S (S Zero) < S (S (S Zero))" ((S (S Zero)) < (S (S (S Zero)))))),
    TestLabel "compare Zero Zero == EQ" 
        (TestCase (assertEqual "compare Zero Zero" EQ (compare Zero Zero)))
  ]

-- Tests per Enum Nat
testsEnumNat :: Test
testsEnumNat = TestList [
    TestLabel "toEnum 0 == Zero" 
        (TestCase (assertEqual "toEnum 0" Zero (toEnum 0))),
    TestLabel "toEnum 3 == S (S (S Zero))" 
        (TestCase (assertEqual "toEnum 3" (S (S (S Zero))) (toEnum 3))),
    TestLabel "fromEnum Zero == 0" 
        (TestCase (assertEqual "fromEnum Zero" 0 (fromEnum Zero))),
    TestLabel "fromEnum (S (S Zero)) == 2" 
        (TestCase (assertEqual "fromEnum (S (S Zero))" 2 (fromEnum (S (S Zero))))),
    TestLabel "succ Zero == S Zero" 
        (TestCase (assertEqual "succ Zero" (S Zero) (succ Zero))),
    TestLabel "pred (S Zero) == Zero" 
        (TestCase (assertEqual "pred (S Zero)" Zero (pred (S Zero))))
  ]

-- Tests per Semigroup Nat
testsSemigroupNat :: Test
testsSemigroupNat = TestList [
    TestLabel "Zero <> Zero == Zero" 
        (TestCase (assertEqual "Zero <> Zero" Zero (Zero <> Zero))),
    TestLabel "Zero <> S Zero == S Zero" 
        (TestCase (assertEqual "Zero <> S Zero" (S Zero) (Zero <> (S Zero)))),
    TestLabel "S Zero <> S Zero == S (S Zero)" 
        (TestCase (assertEqual "S Zero <> S Zero" (S (S Zero)) ((S Zero) <> (S Zero)))),
    TestLabel "Associativitat: (a <> b) <> c == a <> (b <> c)" 
        (TestCase (let a = S Zero; b = S (S Zero); c = S (S (S Zero))
                     in assertEqual "Associativitat" ((a <> b) <> c) (a <> (b <> c))))
  ]

-- Tests per Monoid Nat
testsMonoidNat :: Test
testsMonoidNat = TestList [
    TestLabel "mempty == Zero" 
        (TestCase (assertEqual "mempty" Zero (mempty :: Nat))),
    TestLabel "Identity law esquerra: Zero <> mempty == Zero" 
        (TestCase (assertEqual "Identity esquerra" Zero (Zero <> mempty))),
    TestLabel "Identity law dreta: mempty <> S Zero == S Zero" 
        (TestCase (assertEqual "Identity dreta" (S Zero) (mempty <> (S Zero)))),
    TestLabel "mconcat [Zero, S Zero, S (S Zero)]" 
        (TestCase (assertEqual "mconcat" (S (S (S Zero))) (mconcat [Zero, S Zero, S (S Zero)])))
  ]

-- Tests per Foldable Llista
testsFoldableLlista :: Test
testsFoldableLlista = TestList [
    TestLabel "foldr (+) 0 [1,2,3]" 
        (TestCase (assertEqual "foldr (+) 0" 6 (foldr (+) 0 (L 1 (L 2 (L 3 B)))))),
    TestLabel "foldl (+) 0 [1,2,3]" 
        (TestCase (assertEqual "foldl (+) 0" 6 (foldl (+) 0 (L 1 (L 2 (L 3 B)))))),
    TestLabel "foldMap Sum [1,2,3]" 
        (TestCase (assertEqual "foldMap Sum" (Sum 6) (foldMap Sum (L 1 (L 2 (L 3 B)))))),
    TestLabel "sum [1,2,3]" 
        (TestCase (assertEqual "sum" 6 (sum (L 1 (L 2 (L 3 B)))))),
    TestLabel "product [2,3,4]" 
        (TestCase (assertEqual "product" 24 (product (L 2 (L 3 (L 4 B)))))),
    TestLabel "length [1,2,3]" 
        (TestCase (assertEqual "length" 3 (length (L 1 (L 2 (L 3 B)))))),
    TestLabel "null B és True" 
        (TestCase (assertBool "null B" (null (B :: Llista Int)))),
    TestLabel "null (L 1 B) és False" 
        (TestCase (assertBool "null (L 1 B)" (not (null (L 1 B))))),
    TestLabel "toList [1,2,3]" 
        (TestCase (assertEqual "toList" [1,2,3] (toList (L 1 (L 2 (L 3 B)))))),
    TestLabel "elem 2 [1,2,3]" 
        (TestCase (assertBool "elem 2" (elem 2 (L 1 (L 2 (L 3 B)))))),
    TestLabel "maximum [1,5,3]" 
        (TestCase (assertEqual "maximum" 5 (maximum (L 1 (L 5 (L 3 B)))))),
    TestLabel "minimum [1,5,3]" 
        (TestCase (assertEqual "minimum" 1 (minimum (L 1 (L 5 (L 3 B))))))
  ]

-- Tests per foldNat
testsFoldNat :: Test
testsFoldNat = TestList [
    TestLabel "foldNat (+1) 0 Zero == 0" 
        (TestCase (assertEqual "foldNat (+1) 0 Zero" 0 (foldNat (+1) 0 Zero))),
    TestLabel "foldNat (+1) 0 (S (S Zero)) == 2" 
        (TestCase (assertEqual "foldNat (+1) 0 (S (S Zero))" 2 (foldNat (+1) 0 (S (S Zero))))),
    TestLabel "natAint' Zero == 0" 
        (TestCase (assertEqual "natAint' Zero" 0 (natAint' Zero))),
    TestLabel "natAint' (S (S (S Zero))) == 3" 
        (TestCase (assertEqual "natAint' (S (S (S Zero)))" 3 (natAint' (S (S (S Zero)))))),
    TestLabel "sumNat' Zero (S Zero) == S Zero" 
        (TestCase (assertEqual "sumNat' Zero (S Zero)" (S Zero) (sumNat' Zero (S Zero)))),
    TestLabel "sumNat' (S Zero) (S Zero) == S (S Zero)" 
        (TestCase (assertEqual "sumNat' (S Zero) (S Zero)" (S (S Zero)) (sumNat' (S Zero) (S Zero))))
  ]

-- Tests per exercicis amb llistes
testsExercicisLlistes :: Test
testsExercicisLlistes = TestList [
    TestLabel "maxInt [1,5,3,2]" 
        (TestCase (assertEqual "maxInt" 5 (maxInt [1,5,3,2]))),
    TestLabel "countNothings [Just 1, Nothing, Just 2, Nothing]" 
        (TestCase (assertEqual "countNothings" 2 (countNothings [Just 1, Nothing, Just 2, Nothing]))),
    TestLabel "maxMin [1,5,3,2]" 
        (TestCase (assertEqual "maxMin" (5,1) (maxMin [1,5,3,2]))),
    TestLabel "countSum [1,2,3]" 
        (TestCase (assertEqual "countSum" (3,6) (countSum [1,2,3]))),
    TestLabel "mitjana [1,2,3,4]" 
        (TestCase (assertEqual "mitjana" 2.5 (mitjana [1,2,3,4]))),
    TestLabel "totsMax [2,4,1,4,3,2,1,4]" 
        (TestCase (assertEqual "totsMax" [4,4,4] (totsMax [2,4,1,4,3,2,1,4]))),
    TestLabel "posicionsLletra 'a' \"patata\"" 
        (TestCase (assertEqual "posicionsLletra" [1,3,5] (posicionsLletra 'a' "patata"))),
    TestLabel "sumPar [[1,2],[3,4],[5]]" 
        (TestCase (assertEqual "sumPar" [3,7,5] (sumPar [[1,2],[3,4],[5]]))),
    TestLabel "sumTot [[1,2],[3,4],[5]]" 
        (TestCase (assertEqual "sumTot" 15 (sumTot [[1,2],[3,4],[5]]))),
    TestLabel "separar [3,1,4,1,5] 3" 
        (TestCase (assertEqual "separar" ([1,1,3],[4,5]) (separar [3,1,4,1,5] 3))),
    TestLabel "ordenar [3,1,4,1,5]" 
        (TestCase (assertEqual "ordenar" [1,1,3,4,5] (ordenar [3,1,4,1,5])))
  ]

-- Main suite
mainSuite :: Test
mainSuite = TestList [
    testsIterador,
    testsSumElem,
    testsOrdNat,
    testsEnumNat,
    testsSemigroupNat,
    testsMonoidNat,
    testsFoldableLlista,
    testsFoldNat,
    testsExercicisLlistes
  ]

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

