module Structure where

import Utility

checkNext :: String -> Char
checkNext (x:xs) = x

{-
    A : Lettre et Chiffre en plusieur exemplaire
    a : lettre et chiffre en x1
    L : Lettre only en plusieur exemplaire
    l : lettre only en x1
    = : symbole =
    S : space
    X : forgot the rest
-}

checkCarract :: String -> Bool
checkCarract [x]
     | x >= 'a' && x <= 'z' = True
     | x >= 'A' && x <= 'Z' = True
     | x >= '0' && x <= '9' = True
     | otherwise = False
checkCarract (x:xs)
     | x >= 'a' && x <= 'z' = True
     | x >= 'A' && x <= 'Z' = True
     | x >= '0' && x <= '9' = True
     | otherwise = False

skipSpace :: String -> String
skipSpace [x]
     | x == ' ' = ""
     | otherwise = [x]
skipSpace (x:xs)
     | x == ' ' = ""
     | otherwise = skipAll xs

skipAll :: String -> String
skipAll [x]
     | x >= 'a' && x <= 'z' = ""
     | x >= 'A' && x <= 'Z' = ""
     | x >= '0' && x <= '9' = ""
     | otherwise = [x]
skipAll (x:xs)
     | x >= 'a' && x <= 'z' = skipAll xs
     | x >= 'A' && x <= 'Z' = skipAll xs
     | x >= '0' && x <= '9' = skipAll xs
     | otherwise = (x:xs)

structureAnalyse :: String -> String -> Bool
structureAnalyse [] [c]
     | c == 'X' = True
     | otherwise = False
structureAnalyse [x] [c]
     | c == 'X' = True
     | c == 'A' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9')) = True
     | c == 'l' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z')) = True
     | c == 's' && (x == '+' || x == '-' || x == '%' || x == '/' || x == '*') = True
     | c == '=' && x == '=' = True
     | c == '#' && x == '=' || x == '!' = True
     | x == ' ' = True
     | c == x = True
     | otherwise = False
structureAnalyse (x:xs) [c]
     | c == 'X' = True
     | c == 'A' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9')) = structureAnalyse xs [c]
     | c == '#' && x == '=' || x == '!' = structureAnalyse xs [c]
     | x == ' ' = structureAnalyse xs [c]
     | otherwise = False
structureAnalyse [x] (c:cs)
     | c == 'X' = True
     | c == 'l' = structureAnalyse [] cs
     | c == 'P' && x /= (checkNext cs) = False
     | c == 'P' && x == (checkNext cs) = structureAnalyse [x] cs
     | c == 'A' && ((x >= 'A' && x <= 'Z') ||
                    (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9') ||
                    (x == '(') || (x == ')')) = structureAnalyse [] cs
     | x == c = structureAnalyse [] cs
     | otherwise = False
structureAnalyse (x:xs) (c:cs)
     | c == 'X' = True
     | c == 'P' && x /= (checkNext cs) = structureAnalyse xs (c:cs)
     | c == 'P' && x == (checkNext cs) = structureAnalyse (x:xs) cs {- PPasse les x jusqu'a caractere souhaiter-}
     | c == 'A' && (checkCarract [x] == True) = structureAnalyse (skipAll xs) cs
     | c == 'A' && x == ' ' && (checkCarract xs == True) = structureAnalyse (skipAll xs) cs
     | c == 'A' && (checkCarract [x] == False) = structureAnalyse (skipAll (x:xs)) cs
     | c == 'l' && ((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z')) = structureAnalyse xs cs
     | c == 's' && (x == '+' || x == '-' || x == '%' || x == '/' || x == '*') = structureAnalyse xs cs
     | c == '=' && x == '=' = structureAnalyse xs cs
     | c == '#' && x == '=' || x == '!' = structureAnalyse xs (c:cs)
     | x /= c && x == (checkNext cs) = structureAnalyse (x:xs) cs
     | x /= c && c == (checkNext xs) = structureAnalyse xs (c:cs)
     | x == c = structureAnalyse xs cs
     | x == ' ' = structureAnalyse xs cs
     | otherwise = False

{-      | c == 'A' = structureAnalyse (x:xs) cs

     | x /= c && c == (checkNext xs) = structureAnalyse xs (c:cs)

     -}
researchWords :: String -> String -> String -> Bool
researchWords [xs] (a:as) _ = False
researchWords (x:xs) [a] start
     | x == a = True
     | otherwise = researchWords xs start start
researchWords (x:xs) (a:as) start
     | x == a = researchWords xs as start
     | otherwise = researchWords xs start start