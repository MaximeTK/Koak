module Main where

import System.Environment ( getArgs )
import System.IO
import System.Exit

import Test
import Utility
import DataClass
import Data.Char
import Structure
import Control.Monad (when)
import AnalyseFile
import Builder
import Writer

displayAll :: [String] -> String
displayAll [x] = x
displayAll (x:xs) = myConcat x (displayAll xs)

displayTrees :: [D_Function] -> String
displayTrees [x] = ((f_name x) ++ ":" ++ (f_type x) ++ "\n\t" ++ (a_name (o_1args (f_return x))) ++ " " ++ (charToString (o_operator (f_return x))) ++ " " ++ (a_name (o_2args (f_return x))) ++ "\n")
displayTrees (x:xs) = myConcat ((f_name x) ++ ":" ++ (f_type x) ++ "\n\t" ++ (a_name (o_1args (f_return x))) ++ " " ++ (charToString (o_operator (f_return x))) ++ " " ++ (a_name (o_2args (f_return x))) ++ "\n") (displayTrees xs)

main :: IO ()
main = do
     (x:xs) <- getArgs
     handle <- openFile x ReadMode
     contents <- hGetContents handle
     let info_global = Binary { function = [],file = [] }
     let binaryMake = (convertFileIntoString (myConcat contents "def main():int ;") "" [])
     let convert = (myConcat (writerFile (builder binaryMake []) "") (writerMain (builder binaryMake []) (builder binaryMake []) "step1" 1))
     let file = "main.ll"
     writeFile file convert
     putStrLn (displayTrees (builder binaryMake []))