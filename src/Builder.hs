module Builder where

import Utility
import DataClass
import Data.Char
import Structure

{-REGARDE SI INT OR FLOAT-}
checkType :: String -> String
checkType value
     | researchWords value "." "." = "float"
     | otherwise = "int"

{-ISOLE LA PARTIE QUE L'ON SOUHAITE DANS UNE LIGNE-}
parseFunctionElementStepTwo :: String -> String -> Char -> String
parseFunctionElementStepTwo [x] save stop
     | x /= stop = (myConcat save [x])
     | x == stop = save
parseFunctionElementStepTwo (x:xs) save stop
     | x /= stop = parseFunctionElementStepTwo xs (myConcat save [x]) stop
     | x == stop = save

parseFunctionElement :: String -> Char -> Char -> Bool -> String
parseFunctionElement [x] start stop status
     | x /= start && status == False = ""
     | x == start && status == False = ""
parseFunctionElement (x:xs) start stop status
     | x /= start && status == False = parseFunctionElement xs start stop False
     | x == start && status == False = parseFunctionElementStepTwo xs "" stop

{-ATTRIBUE L'OPERATOR TROUVER DANS LA LIGNE-}
attributeOperator :: String -> Char
attributeOperator function
     | researchWords function "+" "+" = '+'
     | researchWords function "-" "-" = '-'
     | researchWords function "*" "*" = '*'
     | researchWords function "/" "/" = '/'
     | researchWords function "%" "%" = '%'
     | otherwise = 'X'

convertComparator :: String -> Char
convertComparator [x] = x
convertComparator (x:xs) = x

{-ATTRIBUE L'OPERATOR TROUVER DANS LA LIGNE-}
attributeComparator :: String -> String
attributeComparator function
     | researchWords function "<" "<" = "<"
     | researchWords function "<=" "<=" = "<="
     | researchWords function ">" ">" = ">"
     | researchWords function ">=" ">=" = ">="
     | researchWords function "==" "==" = "=="
     | researchWords function "!=" "!=" = "!="
     | otherwise = "X"

isolFunction :: String -> String -> String
isolFunction function code
     | code == "def1" = (myConcat ":" (removeSpaceLine (removeSpaceBeyond (removeUntilStop (removeUntilStop function ')') ':')) "" False))
     | code == "def2" = (myConcat ":" (removeSpaceLine (removeUntilStop function '=') "" False))
     | code == "def3" = (myConcat ":" (removeSpaceLine (removeUntilStop function '=') "" False))
     | code == "if" = (myConcat ":" (removeSpaceLine (removeUntilStop (removeUntilStop function 't') 'n') "" False))
     | code == "while" = (myConcat ":" (removeSpaceLine (removeUntilStop function 'o') "" False))
     | otherwise = ""

checkFunctionType :: [D_Function] -> String -> String
checkFunctionType [x] name
     | f_name x == name = "call"
     | otherwise = "global"
checkFunctionType (x:xs) name
     | f_name x == name = "call"
     | otherwise = checkFunctionType xs name

checkFunctionArgs :: [D_Function] -> String -> String -> String
checkFunctionArgs [x] name code
     | f_name x == name && code == "args1" = a_type (f_1args x)
     | f_name x == name && code == "args2" = a_type (f_2args x)
     | otherwise = "NOTHING"
checkFunctionArgs (x:xs) name code
     | f_name x == name && code == "args1" = a_type (f_1args x)
     | f_name x == name && code == "args2" = a_type (f_2args x)
     | otherwise = checkFunctionArgs xs name code

checkFunctionReturn :: [D_Function] -> String -> D_OperatorFunction
checkFunctionReturn [x] name = (f_return x)
checkFunctionReturn (x:xs) name
     | f_name x == name = (f_return x)
     | otherwise = checkFunctionReturn xs name

{-CREATE RETURN PARTS-}
createReturn :: String -> D_OperatorFunction
createReturn function = OperatorFunction {
     o_returning = assignReturnType function,
     o_1args = createOperatorArgs function "part1" (attributeOperator function),
     o_2args = createOperatorArgs function "part2" (attributeOperator function),
     o_operator = attributeOperator function
}

assignReturnType :: String -> String
assignReturnType value
    | researchWords value "bool" "bool" = "bool"
    | researchWords value "int" "int" = "int"
    | researchWords value "long" "long" = "long"
    | researchWords value "float" "float" = "float"
    | researchWords value "double" "double" = "double"
    | otherwise = "????"

{-CREATE ARGS OF OPERATORS PART-}
createOperatorArgs :: String -> String -> Char -> D_Args
createOperatorArgs function code operator
     | code == "part1" && operator /= 'X' = Args {
          a_name = removeSpaceTotal (parseFunctionElement function ' ' operator False) "",
          a_type = checkType (removeSpaceTotal (parseFunctionElement function ' ' operator False) "") }
     | code == "part2" && operator /= 'X' = Args {
          a_name = removeSpaceTotal (parseFunctionElement function operator ';' False) "",
          a_type = checkType (removeSpaceTotal (parseFunctionElement function operator ';' False) "") }
     | code == "part1" && operator == 'X' = Args {
          a_name = removeSpaceTotal (parseFunctionElement function ' ' ';' False) "",
          a_type = checkType (removeSpaceTotal (parseFunctionElement function ' ' operator False) "") }
     | code == "part2" && operator == 'X' = Args {
          a_name = "",
          a_type = "" }
     | code == "void" = Args {
          a_name = "",
          a_type = "" }
     | otherwise = Args {
          a_name = "ERROR",
          a_type = "ERROR" }

returnFunctionArgsType :: [D_Function] -> String -> String -> String
returnFunctionArgsType [x] name code
     | f_name x == name && code == "args1" = a_type (f_1args x)
     | f_name x == name && code == "args2" = a_type (f_2args x)
     | otherwise = "int"
returnFunctionArgsType (x:xs) name code
     | f_name x == name && code == "args1" = a_type (f_1args x)
     | f_name x == name && code == "args2" = a_type (f_2args x)
     | otherwise = returnFunctionArgsType xs name code

createReturning :: String -> String
createReturning function
     | researchWords function "=" "=" = removeSpaceTotal (parseFunctionElement function ' ' '=' False) ""
     | otherwise = function

{-CREATE ARGS FOR FUNCTION-}
createFunctionArgs :: [D_Function] -> String -> String -> D_Args
createFunctionArgs stock function code
     | code == "f1def1" && (researchWords (parseFunctionElement function '(' ')' False) "," ",") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function '(' ':' False) "",
          a_type = removeSpaceTotal (parseFunctionElement function ':' ',' False) "" }
     | code == "f1def2" && (researchWords (parseFunctionElement function '(' ')' False) "," ",") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function ',' ':' False) "",
          a_type = removeSpaceTotal (parseFunctionElement (removeUntilStop function ',') ':' ')' False) "" }
     | code == "f1def1" = Args {
          a_name = removeSpaceTotal (parseFunctionElement function '(' ':' False) "",
          a_type = removeSpaceTotal (parseFunctionElement function ':' ')' False) "" }
     | code == "f1def2" = Args {
          a_name = "",
          a_type = "" }
     | code == "f2def1" = Args {
          a_name = removeSpaceTotal (parseFunctionElement function ' ' '=' False) "",
          a_type = "int" }
     | code == "call1" && researchWords function "," "," = Args {
          a_name = removeSpaceTotal (parseFunctionElement function '(' ',' False) "",
          a_type = returnFunctionArgsType stock (removeSpaceTotal (parseFunctionElement function '(' ',' False) "") "args1" }
     | code == "call2" && researchWords function "," "," = Args {
          a_name = removeSpaceTotal (parseFunctionElement function ',' ')' False) "",
          a_type = returnFunctionArgsType stock (removeSpaceTotal (parseFunctionElement function ',' ')' False) "") "args2" }
     | code == "call1" = Args {
          a_name = removeSpaceTotal (parseFunctionElement function '(' ')' False) "",
          a_type = returnFunctionArgsType stock (removeSpaceTotal (parseFunctionElement function '(' ')' False) "") "args1" }
     | code == "call2" = Args {
          a_name = "",
          a_type = "" }
     | code == "if1" && (attributeComparator function /= "X") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function 'f' (convertComparator  (attributeComparator function)) False) "",
          a_type = "int" }
     | code == "if2" && (attributeComparator function /= "X") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function (convertComparator  (attributeComparator function)) 't' False) "",
          a_type = "int" }
     | code == "if1" && (attributeComparator function == "X") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function 'f' 't' False) "",
          a_type = "int" }
     | code == "if2" && (attributeComparator function == "X") = Args {
          a_name = "",
          a_type = "" }
     | code == "while1" && (attributeComparator function /= "X") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function 'f' (convertComparator  (attributeComparator (removeUntilStop function '='))) False) "",
          a_type = "int" }
     | code == "while2" && (attributeComparator function /= "X") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function (convertComparator  (attributeComparator (removeUntilStop function '='))) 'd' False) "",
          a_type = "int" }
     | code == "while1" && (attributeComparator function == "X") = Args {
          a_name = removeSpaceTotal (parseFunctionElement function 'e' 'd' False) "",
          a_type = "int" }
     | code == "while2" && (attributeComparator function == "X") = Args {
          a_name = "",
          a_type = "" }
     | code == "" = Args {
          a_name = "",
          a_type = "" }
     | otherwise = Args {
          a_name = "ERROR",
          a_type = "ERROR" }

createFunctionData :: [D_Function] -> String -> String -> D_Function
createFunctionData stock function code
     | code == "def0" = Function {
          f_name = removeSpaceTotal (parseFunctionElement (removeUntilStop function 'f') ' ' '(' False) "",
          f_type = "function",
          f_1args = createFunctionArgs stock function "",
          f_2args = createFunctionArgs stock function "",
          f_comparator = "",
          f_return = createReturn (isolFunction function "def1") }
     | code == "def1" = Function {
          f_name = removeSpaceTotal (parseFunctionElement (removeUntilStop function 'f') ' ' '(' False) "",
          f_type = "function",
          f_1args = createFunctionArgs stock function "f1def1",
          f_2args = createFunctionArgs stock function "f1def2",
          f_comparator = "",
          f_return = createReturn (isolFunction function code) }
     | code == "def2" = Function {
          f_name = removeSpaceTotal (parseFunctionElement (removeUntilStop function 'f') ' ' '=' False) "",
          f_type = "function",
          f_1args = createFunctionArgs stock function "f2def1",
          f_2args = createFunctionArgs stock function "",
          f_comparator = "",
          f_return = createReturn (isolFunction function code) }
     | code == "def3" = Function {
          f_name = removeSpaceTotal (parseFunctionElement (myConcat ":" function) ':' '=' False) "",
          f_type = checkFunctionType stock (removeSpaceTotal (parseFunctionElement (myConcat ":" function) ':' '=' False) ""),
          f_1args = createFunctionArgs stock function "f2def1",
          f_2args = createFunctionArgs stock function "",
          f_comparator = "",
          f_return = createReturn (isolFunction function code) }     
     | code == "call" = Function {
          f_name = removeSpaceTotal (parseFunctionElement (myConcat ":" function) ':' '(' False) "",
          f_type = checkFunctionType stock (removeSpaceTotal (parseFunctionElement (myConcat ":" function) ':' '(' False) ""),
          f_1args = createFunctionArgs stock function "call1",
          f_2args = createFunctionArgs stock function "call2",
          f_comparator = "",
          f_return = checkFunctionReturn stock (removeSpaceTotal (parseFunctionElement (myConcat ":" function) ':' '(' False) "") }
     | code == "if" = Function {
          f_name = "",
          f_type = "if",
          f_1args = createFunctionArgs stock function "if1",
          f_2args = createFunctionArgs stock function "if2",
          f_comparator = (attributeComparator function),
          f_return = createReturn (isolFunction function code) }
     | code == "while" = Function {
          f_name = "",
          f_type = "while",
          f_1args = createFunctionArgs stock function "while1",
          f_2args = createFunctionArgs stock function "while2",
          f_comparator = (attributeComparator function),
          f_return = createReturn (isolFunction function code) }
     | otherwise = Function {
          f_name = "",
          f_type = "",
          f_1args = createFunctionArgs stock function "void",
          f_2args = createFunctionArgs stock function "void",
          f_comparator = "",
          f_return = createReturn function
     }