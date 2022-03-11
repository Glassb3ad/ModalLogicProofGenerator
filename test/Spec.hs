 
import Test.QuickCheck
import PLlogicTreeBuilder
import PLlogicType

main :: IO ()
main = do putStrLn "Test suite not yet implemented"
          putStrLn ("Testing a function that tracks whether plain PL branch closes or not at a certain stage")
          verboseCheck testcheckAtomsForContradictions1
          verboseCheck testcheckAtomsForContradictions2
          verboseCheck testcheckAtomsForContradictions3
          verboseCheck testcheckAtomsForContradictions4
          verboseCheck testcheckAtomsForContradictions5 
          putStrLn "Testing a function that tracks the first non-brancher from memory"
          verboseCheck testGetFirstStraightProposition1
          verboseCheck testGetFirstStraightProposition2
          verboseCheck testGetFirstStraightProposition3
          putStrLn "Testing the function that removes the first non-brancher from memory"
          verboseCheck testCleanOneFromMemory1
          verboseCheck testCleanOneFromMemory2
          verboseCheck testCleanOneFromMemory3
          verboseCheck testCleanOneFromMemory4


testcheckAtomsForContradictions1 :: Property
testcheckAtomsForContradictions1 = checkAtomsForContradictions [] === False

testcheckAtomsForContradictions2 :: Property
testcheckAtomsForContradictions2 = checkAtomsForContradictions [AtomTrue 1, AtomFalse 2, AtomTrue 3] === False

testcheckAtomsForContradictions3 :: Property
testcheckAtomsForContradictions3 = checkAtomsForContradictions [AtomTrue 1, AtomFalse 1, AtomTrue 3] === True

testcheckAtomsForContradictions4 :: Property
testcheckAtomsForContradictions4 = checkAtomsForContradictions [AtomTrue 1, AtomFalse 2, AtomTrue 3, AtomFalse 3] === True 

testcheckAtomsForContradictions5 :: Property
testcheckAtomsForContradictions5 = checkAtomsForContradictions [AtomTrue 1, AtomFalse 2, AtomTrue 3] === False


testGetFirstStraightProposition1 :: Property
testGetFirstStraightProposition1 = getFirstStraightProposition [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 1) (AtomFalse 2) (AtomFalse 2)] === Just (ComplexStraight (ATOM 1) (AtomFalse 2) (AtomFalse 2))

testGetFirstStraightProposition2 :: Property
testGetFirstStraightProposition2 = getFirstStraightProposition [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2)] === Nothing 

testGetFirstStraightProposition3 :: Property
testGetFirstStraightProposition3 = getFirstStraightProposition [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 2) (AtomFalse 2) (AtomFalse 2)] === Just (ComplexStraight (ATOM 1) (AtomFalse 2) (AtomFalse 2))


testCleanOneFromMemory1 :: Property 
testCleanOneFromMemory1 = cleanOneFromMemory [] === []

testCleanOneFromMemory2 :: Property
testCleanOneFromMemory2 = cleanOneFromMemory [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 1) (AtomFalse 2) (AtomFalse 2)] === [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2)]

testCleanOneFromMemory3 :: Property
testCleanOneFromMemory3 = cleanOneFromMemory [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2)] === [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2)]

testCleanOneFromMemory4 :: Property
testCleanOneFromMemory4 = cleanOneFromMemory [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 2) (AtomFalse 2) (AtomFalse 2)] === [ComplexBranches (ATOM 1) (AtomFalse 2) (AtomFalse 2), ComplexStraight (ATOM 2) (AtomFalse 2) (AtomFalse 2)]


