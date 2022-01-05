{-
Program compiler by Ziqi Yang, 2022

This file defines a compiler that firstly 
parses the given MT program as String into AST data structure 
and then generates the corresponding TAM code 
and finally write them into String

MT Program String -> AST -> [TAMInst] -> TAM Code String
-}

module Compiler where
import TAM
import ASTParser
import FunParser
import TAMGenerator

-- read from a MT program file
-- String to TAMInsts to String
compileMT2TAM :: String -> String
compileMT2TAM src = let [(ast, _)] = parse mtProg src in -- parse the given MT program into AST
                    writeTAM (tamCode [] ast)            -- compile the AST into TAM code as String
              
-- writing out a TAM program
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++ show inst ++ "\n") ""