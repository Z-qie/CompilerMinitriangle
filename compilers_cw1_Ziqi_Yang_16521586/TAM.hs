{-
TAM Virtual Machine extended by Ziqi Yang, 2022

The file defines TAM instructions which supports: 
Arithmetic operations, 
Boolean operations, 
Relational operations, 
LOAD and STORE operations
Conditional operations,
IO operations
-}

module TAM where

import Data.List (intercalate)
import State
import FunParser
import Control.Applicative

type MTInt = Int -- TAM Integer type (values in stack)
type StackAddress = MTInt
type LName = String
type Stack = [MTInt]

-- Instructions of the Virtual Machine
data TAMInst
  = LOADL MTInt           -- push an Integer onto the stack
  -- Arithmetic operations
  | ADD                   -- add two values on the top of the stack
  | SUB                   -- subtract second variable on the stack from the top variable
  | MUL                   -- multiplies two values on the top of the stack
  | DIV                   -- divides the second value by the top (integer division)
  | NEG                   -- negates the variable on the top of the stack
  -- Boolean operations
  | AND                   -- Boolean conjunction (non-zero = True)
  | OR                    -- Boolean disjunction
  | NOT                   -- Boolean negation
  -- Relational operations
  | LSS                   -- order operation <
  | GTR                   -- order operation >
  | EQL                   -- equality operator ==
  -- LOAD and STORE operations
  | LOAD StackAddress     -- read the value from address StackAddress of the stack and push the value to the top of the stack
  | STORE StackAddress    -- pop the value from the top of the stack and write to stack address StackAddress
  -- Conditional operations
  | JUMPIFZ LName         -- pop the value from the top of the stack, if 0 jump to Lable LName
  | JUMP LName            -- jump to Lable LName
  | Label LName           -- identify the address of a label LName
  -- IO operations
  | GETINT                -- read integer from terminal and push it onto the top of the stack
  | PUTINT                -- pop the value from the top of the stack and print onto terminal
  | HALT          
  deriving (Eq,Show)



-- Convenient composition operators
-- Pre-composing with a 2-argument function
infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)

-- Implementation of boolean operations on Integers, always return 0 or 1
intAND :: MTInt -> MTInt -> MTInt
intAND = boolInt .< (&&) <. intBool

intOR :: MTInt -> MTInt -> MTInt
intOR = boolInt .< (||) <. intBool

intNOT :: MTInt -> MTInt
intNOT = boolInt . not . intBool

-- Relational operations, return 0 (False) or 1 (True)
intLSS :: MTInt -> MTInt -> MTInt
intLSS = boolInt .< (<)

intGTR :: MTInt -> MTInt -> MTInt
intGTR = boolInt .< (>)

intEQL :: MTInt -> MTInt -> MTInt
intEQL = boolInt .< (==)

-- Correspondence between Booleans and integers
boolInt :: Bool -> MTInt
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: MTInt -> Bool
intBool x = x/=0

-- parsing a TAM program
parseTAMFile :: String -> [TAMInst]
parseTAMFile = pTAM . words where
  pTAM ("LOADL":x:src) = LOADL (read x) : pTAM src
   -- Arithmetic operations
  pTAM ("ADD":src) = ADD : pTAM src
  pTAM ("SUB":src) = SUB : pTAM src
  pTAM ("MUL":src) = MUL : pTAM src
  pTAM ("DIV":src) = DIV : pTAM src
  pTAM ("NEG":src) = NEG : pTAM src
  -- Boolean operations
  pTAM ("AND":src) = AND : pTAM src
  pTAM ("OR" :src) = OR  : pTAM src
  pTAM ("NOT":src) = NOT : pTAM src
  -- Relational operations
  pTAM ("LSS":src) = LSS : pTAM src
  pTAM ("GTR":src) = GTR : pTAM src
  pTAM ("EQL":src) = EQL : pTAM src
  -- LOAD and STORE operations
  pTAM ("LOAD":addr:src) = LOAD (read addr) : pTAM src
  pTAM ("STORE":addr:src) = STORE (read addr) : pTAM src
  -- Conditional operations
  pTAM ("JUMPIFZ":label:src) = JUMPIFZ label : pTAM src
  pTAM ("JUMP":label:src) = JUMP label : pTAM src
  pTAM ("Label":label:src) = Label label : pTAM src
  -- IO operations
  pTAM ("GETINT":src) = GETINT : pTAM src
  pTAM ("PUTINT":src) = PUTINT : pTAM src
  pTAM ("HALT":src) = HALT : pTAM src 
  pTAM _ = []
