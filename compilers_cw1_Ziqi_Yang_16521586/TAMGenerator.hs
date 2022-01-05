{-
TAM Code Generator by Ziqi Yang, 2022

Generation of TAM code from the AST of an Arithmetic expression
Extension with Boolean, Relational, Conditional Expressions
-}

module TAMGenerator where

import TAM
import ASTParser
import State
import Data.Foldable
import Control.Arrow (Arrow(first))

type VarEnv = [(Identifier,StackAddress)]

-- Parse AST into TAM Inst --------------------------------------------------------------------
tamCode :: VarEnv -> AST -> [TAMInst]
-- Program
tamCode ve (Program decls cmd) = declTamInsts ++ cmdTamInsts ++ [HALT]
                      where
                        (ve', declTamInsts) = declsCode decls
                        cmdTamInsts = cmdCode cmd ve'
-- Int value leaf node of AST
tamCode ve (LitInteger a) = [LOADL a]
-- Relational operators that don't have TAM instructions
tamCode ve (BinOp LeqOp t1 t2) =  -- (<=)
  tamCode ve (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2))
tamCode ve (BinOp GeqOp t1 t2) =  -- (>=)
  tamCode ve (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2))
tamCode ve (BinOp NeqOp t1 t2) =  -- (!=)
  tamCode ve (UnOp NegBool (BinOp EqOp t1 t2))
-- Conditional AST Node (double negation to normalize the Boolean value)
tamCode ve (Conditional expr cmd1 cmd2) =
  tamCode ve (BinOp Addition
                (BinOp Multiplication (UnOp NegBool (UnOp NegBool expr)) cmd1)
                (BinOp Multiplication (UnOp NegBool expr) cmd2))
-- General cases
tamCode ve (BinOp op t1 t2) = tamCode ve t1 ++ tamCode ve t2 ++ [binOpTAM op]
tamCode ve (UnOp op t) = tamCode ve t ++ [unOpTAM op]
-- read Var
tamCode ve (Var varName) = [LOAD (getAddr varName ve)]

-- Declarations into TAM Inst --------------------------------------------------------------------
declsCode :: Declarations -> (VarEnv, [TAMInst])
declsCode ds = let (tamInsts, (ve, addr)) = app (declsTAM ds) ([],0) in (ve, tamInsts)

declsTAM :: Declarations -> ST (VarEnv, StackAddress) [TAMInst]
declsTAM [] = return []
declsTAM (d:ds) = do t <- declTAM d
                     ts <- declsTAM ds
                     return (t ++ ts)

declTAM :: Declaration -> ST (VarEnv, StackAddress) [TAMInst]
-- delcaration
declTAM (Decl var) = do (ve, addr) <- stGet
                        if checkDuplication ve var then
                          do stUpdate ((var, addr) : ve, addr + 1)
                             return [LOADL 0] -- initialize as 0
                        else
                          error ("\nCompiling Error: Redefined Variable: Variable \"" ++ var ++ "\" has already been defined!")
-- initialization
declTAM (Init var x) = do (ve, addr) <- stGet
                          if checkDuplication ve var then
                            do stUpdate ((var, addr) : ve, addr + 1)
                               return (tamCode ve x)
                          else
                            error ("\nCompiling Error: Redefined Variable: Variable \"" ++ var ++ "\" has already been defined!")

-- check if redefining/redeclaring a variable at code generation/compile time
checkDuplication :: VarEnv -> Identifier -> Bool
checkDuplication ve var = case lookup var ve of
  Nothing -> True 
  _       -> False

-- Commands into TAM Inst --------------------------------------------------------------------
cmdCode :: Command -> VarEnv -> [TAMInst]
-- label index starts from number 0
cmdCode cmd ve = let (tamInsts, _) = app (cmdTAM cmd ve) 0 in tamInsts

cmdTAM :: Command -> VarEnv -> ST Int [TAMInst]
-- Assignment: var = expr
cmdTAM (Assignment var expr) ve = return (value ++ [STORE addr])
                                    where
                                      addr = getAddr var ve
                                      value = tamCode ve expr
-- If expr Then cmd1 Else cmd2
cmdTAM (IfThenElse expr cmd1 cmd2) ve = do label1 <- fresh
                                           label2 <- fresh
                                           tamCmd1 <- cmdTAM cmd1 ve
                                           tamCmd2 <- cmdTAM cmd2 ve
                                           let tamExpr = tamCode ve expr
                                           return (tamExpr ++ [JUMPIFZ label1] ++ tamCmd1 ++ [JUMP label2, Label label1] ++ tamCmd2 ++ [Label label2])
-- While expr do cmd
cmdTAM (WhileDo expr cmd) ve = do label1 <- fresh
                                  label2 <- fresh
                                  tamCmd <- cmdTAM cmd ve
                                  let tamExpr = tamCode ve expr
                                  return ([Label label1] ++ tamExpr ++ [JUMPIFZ label2] ++ tamCmd ++ [JUMP label1, Label label2])
-- GETINT
cmdTAM (GetInt varName) ve = return [GETINT, STORE addr]
                              where
                                addr = getAddr varName ve
-- PUTINT
cmdTAM (PrintInt expr) ve = return (tamExpr ++ [PUTINT])
                              where
                                tamExpr = tamCode ve expr
-- BEGIN END
cmdTAM (BeginEnd cmds) ve = cmdsTAM cmds ve


cmdsTAM :: Commands -> VarEnv -> ST Int [TAMInst]
cmdsTAM [] _ = return []
cmdsTAM (cmd : cmds) ve = do tamInst <- cmdTAM cmd ve
                             tamInsts <- cmdsTAM cmds ve
                             return (tamInst ++ tamInsts)

------------------------------------------------------------------------------------------------------------------
fresh :: ST Int String
fresh = do n <- stGet
           stUpdate (n + 1)
           return ('#' : show n)
-- fresh sintStr = sintStr >>= \n -> stGet >>= \n -> S(\_ -> ((), (n + 1)) >>= \m -> return ('#' : (show m))

-- get the stack address by given var name
getAddr :: String -> VarEnv -> StackAddress
getAddr var ve = case findAddr var ve of
                    Just addr -> addr
                    Nothing -> error ("\nCompiling Error: Variable " ++ var ++ " does not exist!")

findAddr :: String -> VarEnv -> Maybe StackAddress
findAddr var ve = do (str, addr) <- find (\(str, _) -> str == var) ve
                     return addr

binOpTAM :: BinOperator -> TAMInst
binOpTAM Addition       = ADD
binOpTAM Subtraction    = SUB
binOpTAM Multiplication = MUL
binOpTAM Division       = DIV
binOpTAM Conjunction    = AND
binOpTAM Disjunction    = OR
binOpTAM LssOp          = LSS
binOpTAM GtrOp          = GTR
binOpTAM EqOp           = EQL
binOpTAM a              = error ("\nCompiling Error: Undefined TAMInst: \"" ++ show a ++ "\"!")

unOpTAM :: UnOperator -> TAMInst
unOpTAM Negation = NEG
unOpTAM NegBool  = NOT
