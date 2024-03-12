module DataClass where

data D_Args = Args {
     a_name :: String,
     a_type :: String
} deriving Show

data D_OperatorFunction = OperatorFunction {
     o_returning :: String,
     o_1args :: D_Args,
     o_2args :: D_Args,
     o_operator :: Char
} deriving Show

data D_Function = Function {
     f_name :: String,
     f_type :: String,
     f_1args :: D_Args,
     f_2args :: D_Args,
     f_comparator :: String,
     f_return :: D_OperatorFunction
} deriving Show

data D_Binary = Binary { 
    function :: [D_Function],
    file :: [String]
} deriving Show