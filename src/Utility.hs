module Utility where

{-CONCAT EVERYTHING-}
myConcat :: [a] -> [a] -> [a]
myConcat xs ys = foldr (:) ys xs

{-RETIRE TOUT LES ESPACES EN DOUBLE-}
removeSpaceLine :: String -> String -> Bool -> String
removeSpaceLine [x] stock status
     | x /= ' ' && x /= '\t' = myConcat stock [x]
     | otherwise = stock
removeSpaceLine (x:xs) stock status
     | x /= ' ' && x /= '\t' = removeSpaceLine xs (myConcat stock [x]) False
     | status == False && (x == ' ' || x == '\t') = removeSpaceLine xs (myConcat stock [x]) True
     | otherwise = removeSpaceLine xs stock True

{-RETIRE TOUT LES CARACTERE JUSQU'A TROUVÉ LE CHARACTER VOULU-}
removeUntilStop :: String -> Char -> String
removeUntilStop [x] stop
     | x /= stop = ""
     | otherwise = ""
removeUntilStop (x:xs) stop
     | x /= stop = removeUntilStop xs stop
     | otherwise = xs

{-RETIRE TOUT LES ESPACES AVANT-}
removeSpaceBeyond :: String -> String
removeSpaceBeyond [x]
     | x == ' ' || x == '\t' = ""
     | otherwise = [x]
removeSpaceBeyond (x:xs)
     | x == ' ' || x == '\t' = removeSpaceBeyond xs
     | otherwise = (x:xs)

{-RETIRE TOUT LES ESPACES D'UNE STRING-}
removeSpaceTotal :: String -> String -> String
removeSpaceTotal [] save = save
removeSpaceTotal [x] save
     | x == ' ' || x == '\t' = save
     | otherwise = (myConcat save [x])
removeSpaceTotal (x:xs) save
     | x == ' ' || x == '\t' = removeSpaceTotal xs save
     | otherwise = removeSpaceTotal xs (myConcat save [x])


convertFileIntoString :: String -> String -> [String] -> [String]
convertFileIntoString [x] stock list 
     | x /= '\n' = (myConcat list [(myConcat stock [x])])
     | otherwise = (myConcat list [stock])
convertFileIntoString (x:xs) stock list
     | x /= '\n' = convertFileIntoString xs (myConcat stock [x]) list
     | x == '\n' = convertFileIntoString xs "" (myConcat list [stock])
     | otherwise = list

cropString :: String -> String
cropString function = (removeSpaceBeyond (removeUntilStop (removeUntilStop function ')') ':'))

strInt :: String -> Int 
strInt str = read str :: Int 

charToString :: Char -> String
charToString x = [x]