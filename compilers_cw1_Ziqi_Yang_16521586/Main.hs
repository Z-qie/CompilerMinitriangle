
{-
Main executable extended by Ziqi Yang, 2022

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

A Compiler and evaluator for Arithmetic expressions with booleans,
relations, and conditional expressions.
Compiles simple Arith Expr into TAM programs.
-}

module Main where
import Compiler
import ASTParser
import TAM
import State
import FunParser
import TAMExecutor

import System.Environment
import Data.Char

data FileType = TAM | MT
  deriving (Eq,Show)

main :: IO ()
main = do args <- getArgs
          mainWithArgs args

mainWithArgs :: [String] -> IO()
mainWithArgs args = do
  let inputName = head args
  let (fileName,extension) = fileNE args

  -- execute the parsed TAM code
  let tamFun :: [TAMInst] -> IO Stack
      tamFun tamInsts = case tamInsts of
                          [] -> error "Invalid TAM File!"
                          _  -> do putStrLn "Executing TAM code..."
                                   executeTAMCode tamInsts

-- If input file has extension .tm, compile the expression to TAM code
-- If input file has extension .tam, execute tam code     
  case extension of
    TAM -> do src <- readFile (fileName ++ ".tam")
              stk <- tamFun (parseTAMFile src)
              putStrLn ("Remaining TAM stack (variables): " ++ show stk)
    MT -> do src <- readFile (fileName ++ ".mt")
             writeFile (fileName ++ ".tam") (compileMT2TAM src)
             >> putStrLn ("MT program has been compiled to TAM file: " ++ fileName ++ ".tam")


-- Finding the base name and extension of a file name
baseName :: String -> String
baseName = takeWhile (/='.')

fileExt :: String -> String
fileExt fn = let ext = dropWhile (/='.') fn
             in if ext == "" then ".exp" else ext

extType :: String -> Maybe FileType
extType ".tam" = Just TAM
extType ".mt" = Just MT
extType _ = Nothing

parseFileName :: String -> Maybe (String,FileType)
parseFileName arg = do
  if isAlpha (head arg)
    then let name = baseName arg
             ext  = extType (fileExt arg)
         in case ext of
              Just t -> Just (name,t)
              Nothing -> Nothing
    else Nothing

unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

-- We assume one of the arguments is a file name
fileNE :: [String] -> (String, FileType)
fileNE = head . unJust . map parseFileName

