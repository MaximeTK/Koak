module Writer where

import DataClass
import Utility
import Builder
{-
define dso_local i32 @test2(i32 %0, i32 %1) #0 {
-}

convertType :: String -> String
convertType value
    | value == "bool" = "i8"
    | value == "int" = "i32"
    | value == "long" = "i64"
    | value == "float" = "float"
    | value == "double" = "double"
    | otherwise = "|" ++ value ++ "|"

convertAlign :: String -> String
convertAlign value
    | value == "bool" = "align 1"
    | value == "int" = "align 4"
    | value == "long" = "align 8"
    | value == "float" = "align 4"
    | value == "double" = "align 8"
    | otherwise = ""

checkArgs :: D_Function -> String -> D_Args
checkArgs function code
    | code == "analyse1" && (a_name (f_1args function) == (a_name (o_1args (f_return function)))) = (f_1args function)
    | code == "analyse1" && (a_name (f_2args function) == (a_name (o_1args (f_return function)))) = (f_2args function)
    | code == "analyse1" = (o_1args (f_return function))
    | code == "analyse2" && (a_name (f_1args function) == (a_name (o_2args (f_return function)))) = (f_1args function)
    | code == "analyse2" && (a_name (f_2args function) == (a_name (o_2args (f_return function)))) = (f_2args function)
    | code == "analyse2" = (o_2args (f_return function))
    | otherwise = f_2args function

checkOperator :: D_Function -> String -> Bool
checkOperator function code
    | code == "analyse1" && (a_name (o_1args (f_return function))) /= "" = True
    | code == "analyse2" && (a_name (o_1args (f_return function))) /= "" = True
    | otherwise = False

writeReturn :: D_Function -> Int -> String
writeReturn function compteur = "  ret " ++ (convertType (o_returning (f_return function))) ++ " %" ++ (show (compteur) :: String) ++ "\n}\n\n"

writeOperation :: D_Function -> D_Args -> D_Args -> String -> Int -> String
writeOperation function args1 args2 code compteur
    | code == "+" && a_type args1 == "int" && a_type args2 == "int" = "  %" ++ (show compteur :: String) ++ " = add nsw i32 %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function compteur)
    | code == "+" && a_type args1 == "long" && a_type args2 == "long" = "  %" ++ (show compteur :: String) ++ " = add nsw i64 %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "+" && a_type args1 == "float" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fadd float %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "+" && a_type args1 == "double" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fadd double %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "+" && a_type args1 == "float" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fpext float %4 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fadd double %7, %6\n" ++ (writeReturn function (compteur + 1))
    | code == "+" && a_type args1 == "double" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fpext float %6 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fadd double %4, %7\n" ++ (writeReturn function (compteur + 1))

    | code == "-" && a_type args1 == "int" && a_type args2 == "int" = "  %" ++ (show compteur :: String) ++ " = sub nsw i32 %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "-" && a_type args1 == "long" && a_type args2 == "long" = "  %" ++ (show compteur :: String) ++ " = sub nsw i64 %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "-" && a_type args1 == "float" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fsub float %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "-" && a_type args1 == "double" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fsub double %3, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "-" && a_type args1 == "float" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fpext float %4 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fsub double %7, %6\n" ++ (writeReturn function (compteur + 1))
    | code == "-" && a_type args1 == "double" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fpext float %6 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fsub double %4, %7\n" ++ (writeReturn function (compteur + 1))
    
    | code == "*" && a_type args1 == "int" && a_type args2 == "int" = "  %" ++ (show compteur :: String) ++ " = mul nsw i32 %4, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "*" && a_type args1 == "long" && a_type args2 == "long" = "  %" ++ (show compteur :: String) ++ " = mul nsw i64 %4, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "*" && a_type args1 == "float" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fmul float %4, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "*" && a_type args1 == "double" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fmul double %4, " ++ a_name args2 ++ "\n" ++ (writeReturn function (compteur))
    | code == "*" && a_type args1 == "float" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fpext float %4 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fmul %7, %6\n" ++ (writeReturn function (compteur + 1))
    | code == "*" && a_type args1 == "double" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fpext float %6 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fmul %4, %7\n" ++ (writeReturn function (compteur + 1))
    
    | code == "/" && a_type args1 == "int" && a_type args2 == "int" = "  %" ++ (show compteur :: String) ++ "%" ++ (show compteur :: String) ++ " = sdiv i32 %4, " ++ a_name args2 ++ "\n"
    | code == "/" && a_type args1 == "float" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ "%" ++ (show compteur :: String) ++ " = sdiv i64 %4, " ++ a_name args2 ++ "\n"
    | code == "/" && a_type args1 == "float" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fdiv float %4, %6\n" ++ (writeReturn function (compteur))
    | code == "/" && a_type args1 == "double" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fdiv double %4, %6\n" ++ (writeReturn function (compteur))
    | code == "/" && a_type args1 == "float" && a_type args2 == "double" = "  %" ++ (show compteur :: String) ++ " = fpext float %4 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fdiv %7, %6\n" ++ (writeReturn function (compteur + 1))
    | code == "/" && a_type args1 == "double" && a_type args2 == "float" = "  %" ++ (show compteur :: String) ++ " = fpext float %6 to double\n" ++ "  %" ++ (show (compteur + 1) :: String) ++ " = fdiv %4, %7\n" ++ (writeReturn function (compteur + 1))
    
    | code == "%" && a_type args1 == "int" && a_type args2 == "int" = "  %" ++ (show compteur :: String) ++ " = srem %4, %6\n" ++ (writeReturn function (compteur))
    | code == "%" && a_type args1 == "long" && a_type args2 == "long" = "  %" ++ (show compteur :: String) ++ " = srem %4, %6\n" ++ (writeReturn function (compteur))
    | otherwise = ""

{-incremente le compteur-}
addCompteur :: String -> Int -> Int
addCompteur action compteur
    | action == "defalloc2args" = compteur + 3
    | action == "defalloc1args" = compteur + 2
    | action == "alloc2args" = compteur + 2
    | action == "alloc1args" = compteur + 1
    | otherwise = compteur + 0

writeAllocation :: D_Args -> D_Args -> String -> Int -> String
writeAllocation args1 args2 code compteur
    | a_name args1 == "" && a_name args2 == "" = ""
    | a_name args1 /= "" && a_name args2 == "" && code == "args1" = "  %" ++ (show compteur :: String) ++ " = alloca " ++ (convertType (a_type args1)) ++ ", " ++ (convertAlign (a_type args1)) ++ "\n" 
        ++ "  store " ++ (convertType (a_type args1)) ++ " %0, " ++ (convertType (a_type args1)) ++ "* %" ++ (show compteur :: String) ++ ", " ++ (convertAlign (a_type args1)) ++ "\n"
        ++ "  %" ++ (show (compteur + 1) :: String) ++ " = load " ++ (convertType (a_type args1)) ++ ", " ++ (convertType (a_type args1)) ++ "* %" ++ (show compteur :: String) ++ ", " ++ (convertAlign (a_type args1)) ++ "\n"
    | a_name args1 /= "" && a_name args2 == "" && code == "args2" = ""
    | code == "args1" = "  %" ++ (show compteur :: String) ++ " = alloca " ++ (convertType (a_type args1)) ++ ", " ++ (convertAlign (a_type args1)) ++ "\n" 
        ++ "  store " ++ (convertType (a_type args1)) ++ " %0, " ++ (convertType (a_type args1)) ++ "* %" ++ (show compteur :: String) ++ ", " ++ (convertAlign (a_type args1)) ++ "\n"
        ++ "  %" ++ (show (compteur + 1) :: String) ++ " = load " ++ (convertType (a_type args1)) ++ ", " ++ (convertType (a_type args1)) ++ "* %" ++ (show compteur :: String) ++ ", " ++ (convertAlign (a_type args1)) ++ "\n"
    | code == "args2" = ""
    | otherwise = ""

writerbody :: D_Function -> Int -> String
writerbody function compteur
    | (checkOperator function "analyse2" == True) = (writeAllocation (checkArgs function "analyse1") (checkArgs function "analyse2") "args1" compteur) 
        ++ (writeAllocation (checkArgs function "analyse1") (checkArgs function "analyse2") "args2" (addCompteur "alloc2args" compteur))
        ++ (writeOperation function (checkArgs function "analyse1") (checkArgs function "analyse2") (charToString (o_operator (f_return function))) (addCompteur "alloc2args" compteur))
    | otherwise = 
           (writeAllocation (checkArgs function "analyse1") (checkArgs function "analyse2") "args1" compteur)
        ++ (writeOperation function (checkArgs function "analyse1") (checkArgs function "analyse2") (charToString (o_operator (f_return function))) (addCompteur "alloc1args" compteur))
        ++ (writeReturn function (addCompteur "alloc1args" compteur))
{-ECRIT LA DEF DE LA FUNCTION-}

writeFunctionInitialisation :: D_Function -> Int -> String
writeFunctionInitialisation function compteur
{-2 ARGS-}
    | f_type function == "global" = "@" ++ (f_name function) ++ " = global " ++ (convertType (a_type (o_1args (f_return function)))) ++ " " ++ (a_name (o_1args (f_return function))) ++ ", " ++ (convertAlign (a_type (o_1args (f_return function)))) ++ "\n\n"
    | f_type function == "call" = ""
    | (a_name (f_1args function)) /= "" && (a_name (f_2args function)) /= "" = "define " 
        ++ (convertType (o_returning (f_return function))) ++ " @"
        ++ (f_name function) ++ "(" ++ (convertType (a_type (f_1args function))) ++ " %" ++ (show compteur :: String) ++ ", " 
        ++ (convertType (a_type (f_2args function))) ++ " %" ++ (show (compteur + 1) :: String) ++ ") #0 {\n" 
        ++ (writerbody function (addCompteur "defalloc2args" compteur))
    | (a_name (f_1args function)) /= "" && (a_name (o_1args (f_return function))) /= "" && ((a_name (f_1args function)) /= (a_name (o_1args (f_return function)))) = "define " 
        ++ (convertType (o_returning (f_return function))) ++ " @"
        ++ (f_name function) ++ "(" ++ (convertType (a_type (f_1args function))) ++ " %" ++ (show compteur :: String) ++ ") #0 {\n" 
        ++ (writerbody function (addCompteur "defalloc1args" compteur))    
    | (a_name (f_1args function)) /= "" && (a_name (o_2args (f_return function))) /= "" && ((a_name (f_1args function)) /= (a_name (o_2args (f_return function)))) = "define " 
        ++ (convertType (o_returning (f_return function))) ++ " @"
        ++ (f_name function) ++ "(" ++ (convertType (a_type (f_1args function))) ++ " %" ++ (show compteur :: String) ++ ") #0 {\n" 
        ++ (writerbody function (addCompteur "defalloc1args" compteur))
    | (a_name (f_1args function)) /= "" = "define " ++ (convertType (o_returning (f_return function))) ++ " @" 
        ++ (f_name function) ++ "(" ++ (convertType (a_type (f_1args function)))
        ++ " %" ++ (show compteur :: String) ++ ") #0 {\n" ++ (writerbody function (addCompteur "defalloc1args" compteur))
    |otherwise = ""

writerMain :: [D_Function] -> [D_Function] -> String -> Int -> String
writerMain [x] stock code compteur
    | code == "step1" = myConcat "define i32 @main() #0 {\n" (writerMain [x] stock "step2" compteur)
    | code == "step2" && f_type x == "call" = "call " ++ convertType (o_returning (f_return x)) ++ " @" ++ f_name x ++ "(" ++ (convertType (a_type (f_1args x))) ++ " " ++ (a_name (f_1args x)) ++ " ," ++ (convertType (a_type (f_2args x))) ++ " " ++ (a_name (f_2args x)) ++ ")"
    | otherwise = "  ret i32 %" ++ (show (compteur - 1) :: String) ++ "\n}\n"
writerMain (x:xs) stock code compteur
    | code == "step1" = myConcat "define i32 @main() #0 {\n" (writerMain (x:xs) stock "step2" compteur)
    | code == "step2" && f_type x == "call" = myConcat ("  %" ++ (show compteur :: String) ++ " = call " 
        ++ convertType (o_returning (f_return x)) ++ " @" ++ f_name x ++ "(" ++ convertType (checkFunctionArgs stock (f_name x) "args1")
        ++ " " ++ (a_name (f_1args x)) ++ ")\n") (writerMain xs stock "step2" (compteur + 1))
    | otherwise = writerMain xs stock code compteur

writerFile :: [D_Function] -> String -> String
writerFile [] stock = ""
writerFile [x] stock
    | f_name x /= "" = myConcat stock (writeFunctionInitialisation x 0)
    | otherwise = stock
writerFile (x:xs) stock
    | f_name x /= "" = writerFile xs (myConcat stock (writeFunctionInitialisation x 0))
    | otherwise = writerFile xs stock