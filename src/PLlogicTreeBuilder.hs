module PLlogicTreeBuilder where

import PLlogicType


data ProofTree = ComplexStraight Proposition ProofTree ProofTree
                | ComplexBranches Proposition ProofTree ProofTree
                | AtomTrue Int
                | AtomFalse Int
    deriving (Show, Eq)

buildTree :: Proposition -> ProofTree
buildTree (ATOM n) = AtomTrue n
buildTree (NOT (ATOM n)) = AtomFalse n
buildTree (NOT (NOT a)) = buildTree a

buildTree (AND a b) = ComplexStraight (AND a b) (buildTree a) (buildTree b)
buildTree (NOT (AND a b)) = ComplexBranches (NOT (AND a b)) (buildTree (NOT a)) (buildTree (NOT b))

buildTree (OR a b) = ComplexBranches (OR a b) (buildTree a) (buildTree b)
buildTree (NOT (OR a b)) = ComplexStraight (NOT (OR a b)) (buildTree (NOT a)) (buildTree (NOT b))

buildTree (IF a b) = ComplexBranches (IF a b) (buildTree (NOT a)) (buildTree b)
buildTree (NOT (IF a b)) = ComplexStraight (NOT (IF a b)) (buildTree a) (buildTree (NOT b))

buildTree (PLlogicType.EQ a b) = ComplexBranches (PLlogicType.EQ a b) (buildTree (AND a b)) (buildTree (AND (NOT a) (NOT b)))
buildTree (NOT (PLlogicType.EQ a b)) = ComplexBranches (NOT (PLlogicType.EQ a b)) (buildTree (AND (NOT a) b)) (buildTree (AND a (NOT b)))

checkValidity :: Proposition -> (Bool, [ProofTree])
checkValidity p =
                  if null x
                  then (True, [])
                  else (False, x)
                  where x = checkCounterExampleFromTree (buildTree p) [] []

checkCounterExampleFromTree :: ProofTree -> [ProofTree] -> [ProofTree] -> [ProofTree]
checkCounterExampleFromTree x y c = []
--checkValidityFromTree (ComplexBranches p treeA treeB) atoms = checkValidityFromTree treeA atoms && checkValidityFromTree treeB atoms
-- checkValidityFromTree (ComplexStraight p treeA treeB) atoms = ??? 




--getFirstStraightProposition function checks whether memory contain propositions that do not introduce branches and return first non brancher.

getFirstStraightProposition :: [ProofTree] -> Maybe ProofTree
getFirstStraightProposition [] = Nothing
getFirstStraightProposition (ComplexStraight x y z : memory) = Just (ComplexStraight x y z)
getFirstStraightProposition (x:memory) = getFirstStraightProposition memory

-- cleanOneFromMemory function removes the first propositions that do not introduce branches. If none is found memory stays intact
cleanOneFromMemory :: [ProofTree] -> [ProofTree]
cleanOneFromMemory [] = []
cleanOneFromMemory (ComplexStraight x y z : memory) = memory
cleanOneFromMemory (x:xs) = x : cleanOneFromMemory xs

-- checkAtomsForContradictions -function is used to check whether certain branch closes or not.
-- Function divies proposition into two lists. 
-- First contains atoms with positive affliction (eg. P), the second atoms with negative afflictions (eq. Not P). 
-- If any atom is found from both lists, then function return true.
checkAtomsForContradictions :: [ProofTree] -> Bool
checkAtomsForContradictions atoms = checkIfListsShareAMember (getTruePropositions atoms) (getFalsePropositions atoms)

checkIfListsShareAMember :: [Int] -> [Int] -> Bool
checkIfListsShareAMember (x:xs) ys = checkIfListIncludes x ys || checkIfListsShareAMember xs ys
checkIfListsShareAMember [] ys = False

checkIfListIncludes :: Int -> [Int] -> Bool
checkIfListIncludes x [] = False
checkIfListIncludes x (y:ys)
    | x == y = True
    | otherwise = checkIfListIncludes x ys

getTruePropositions :: [ProofTree] -> [Int]
getTruePropositions ((AtomTrue n) : atoms) = n : getTruePropositions atoms
getTruePropositions (n : atoms) = getTruePropositions atoms
getTruePropositions [] = []

getFalsePropositions :: [ProofTree] -> [Int]
getFalsePropositions ((AtomFalse n) : atoms) = n : getFalsePropositions atoms
getFalsePropositions (n : atoms) = getFalsePropositions atoms
getFalsePropositions [] = []

--------------------------------------------------------------------------

getValue :: Proposition -> Bool
getValue (ATOM n) = False
getValue (ATOM n) = False
getValue (NOT a) = not (getValue a)
getValue (AND a b) = getValue a && getValue b
getValue (OR a b) = getValue a || getValue b
getValue (IF a b) = not (getValue a) || getValue b
getValue (PLlogicType.EQ a b) = getValue (IF a b) && getValue (IF b a)
