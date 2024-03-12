module Test where
import Structure
import Utility

structureAnalyseTest :: String -> String -> String
structureAnalyseTest [] [c]
     | c == 'X' = "True"
     | otherwise = "False1"
structureAnalyseTest [x] [c]
     | c == 'X' = "True"
     | c == 'A' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9')) = "True"
     | c == 'l' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z')) = "True"
     | c == 's' && (x == '+' || x == '-' || x == '%' || x == '/' || x == '*') = "True"
     | c == '=' && x == '=' = "True"
     | c == '#' && x == '=' || x == '!' = "True"
     | x == ' ' = "True"
     | c == x = "True"
     | otherwise = "False2"
structureAnalyseTest (x:xs) [c]
     | c == 'X' = "True"
     | c == 'A' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9')) = structureAnalyseTest xs [c]
     | c == '#' && x == '=' || x == '!' = structureAnalyseTest xs [c]
     | x == ' ' = structureAnalyseTest xs [c]
     | otherwise = "False3"
structureAnalyseTest [x] (c:cs)
     | c == 'X' = "True"
     | c == 'l' = structureAnalyseTest [] cs
     | c == 'P' && x /= (checkNext cs) = "False4"
     | c == 'P' && x == (checkNext cs) = structureAnalyseTest [x] cs
     | c == 'A' && ((x >= 'A' && x <= 'Z') ||
                    (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9') ||
                    (x == '(') || (x == ')')) = structureAnalyseTest [] cs
     | x == c = structureAnalyseTest [] cs
     | otherwise = "False5"
structureAnalyseTest (x:xs) (c:cs)
     | c == 'X' = "True"
     | c == 'P' && x /= (checkNext cs) = structureAnalyseTest xs (c:cs)
     | c == 'P' && x == (checkNext cs) = structureAnalyseTest (x:xs) cs {- PPasse les x jusqu'a caractere souhaiter-}
     | c == 'A' && (checkCarract [x] == True) = structureAnalyseTest (skipAll xs) cs
     | c == 'A' && x == ' ' && (checkCarract xs == True) = structureAnalyseTest (skipAll xs) cs
     | c == 'A' && (checkCarract [x] == False) = structureAnalyseTest (skipAll (x:xs)) cs
     | c == 'l' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z')) = structureAnalyseTest xs cs
     | c == 's' && (x == '+' || x == '-' || x == '%' || x == '/' || x == '*') = structureAnalyseTest xs cs
     | c == '=' && x == '=' = structureAnalyseTest xs cs
     | c == '#' && x == '=' || x == '!' = structureAnalyseTest xs (c:cs)
     | x /= c && x == (checkNext cs) = structureAnalyseTest (x:xs) cs
     | x /= c && c == (checkNext xs) = structureAnalyseTest xs (c:cs)
     | x == c = structureAnalyseTest xs cs
     | x == ' ' = structureAnalyseTest xs cs
     | otherwise = myConcat (myConcat (x:xs) "|") (c:cs)
