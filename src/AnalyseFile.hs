module AnalyseFile where

import Builder
import DataClass
import Data.Char
import Utility
import Structure

{-ANALYSE QUELLE GENRE DE IF IL S'AGIT-}
analyseIF :: String -> Bool
analyseIF x
     | structureAnalyse x "if lA>A then AP;" = True
     | structureAnalyse x "if lA>=A then AP;" = True
     | structureAnalyse x "if lA<A then AP;" = True
     | structureAnalyse x "if lA<=A then AP;" = True
     | structureAnalyse x "if lA==A then AP;" = True
     | structureAnalyse x "if lA!=A then AP;" = True
     | otherwise = False

{-ANALYSE QUELLE GENRE DE WHILE IL S'AGIT-}
analyseWhile :: String -> Bool
analyseWhile x
     | structureAnalyse x "while lA>A do AP;" = True
     | structureAnalyse x "while lA>=A do AP;" = True
     | structureAnalyse x "while lA<A do AP;" = True
     | structureAnalyse x "while lA<=A do AP;" = True
     | structureAnalyse x "while lA==A do AP;" = True
     | structureAnalyse x "while lA!=A do AP;" = True
     | otherwise = False

{-ANALYSE QUELLE GENRE DE FOR IL S'AGIT-}
analyseFor :: String -> Bool
analyseFor x
     | structureAnalyse x "for lA=A,lA>Ado AP;" = True
     | structureAnalyse x "for lA=A,lA>=Ado AP;" = True
     | structureAnalyse x "for lA=A,lA<Ado AP;" = True
     | structureAnalyse x "for lA=A,lA<=Ado AP;" = True
     | structureAnalyse x "for lA=A,lA==Ado AP;" = True
     | structureAnalyse x "for lA=A,lA!=Ado AP;" = True
     | otherwise = False

{-ANALYSE LA STRUCTURE-}
analyseStructure :: [D_Function] -> String -> String -> D_Function
analyseStructure stock x code
     | code == "def" && (structureAnalyse x "def lA():P;" == True) = createFunctionData stock x "def0"
     | code == "def" && (structureAnalyse x "def lA(lP:P):P;" == True) = createFunctionData stock x "def1"
     | code == "def" && (structureAnalyse x "def lP=P;" == True) = createFunctionData stock (removeUntilStop x ' ') "def3"
     | code == "hiddendef" && (structureAnalyse x "lP=P;" == True) = createFunctionData stock (myConcat " " x) "def3"
     | code == "hiddendef" && (structureAnalyse x "lP(P);" == True) = createFunctionData stock (myConcat " " x) "call"
     | code == "if" && (analyseIF x) = createFunctionData stock x "if"
     | code == "while" && (analyseWhile x) = createFunctionData stock x "while"
     | otherwise = createFunctionData stock x "error creating"

{-ANALYSE QUELLE GENRE DE COMMANDE IL S'AGIT-}
analyseString :: [D_Function] -> String -> D_Function
analyseString stock x
     | researchWords x "def" "def" = analyseStructure stock x "def" 
     | researchWords x "if" "if" = analyseStructure stock x "if"
     | researchWords x "while" "while" = analyseStructure stock x "while"
     | researchWords x "for" "for" = analyseStructure stock x "for"
     | otherwise = analyseStructure stock x "hiddendef"

builder :: [String] -> [D_Function] -> [D_Function]
builder [x] stock = (myConcat stock [(analyseString stock x)])
builder (x:xs) stock = builder xs (myConcat stock [(analyseString stock x)])