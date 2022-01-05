{-
TAM Code Executor by Ziqi Yang, 2022

This file defines a TAMValue record and 
TAMSt monad (can handle IO) used when executing each instrution.

Error check for input is implemented.
-}
module TAMExecutor where

import Data.List
import Data.Maybe
import State
import System.IO
import TAM
import Text.Read (readMaybe)

-- record for TAMValue
data TAMValue = TAMValue {
    tsCode :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack
} deriving (Eq, Show)

type TAMSt a = StateIO TAMValue a

-- execute given TAM code with Stack updated
executeTAMCode :: [TAMInst] -> IO Stack
executeTAMCode tamInsts = do (stk, _) <- appIO executeT (initTAMState tamInsts)
                             return stk

-- initializa the default TAM record to run the TAM code
initTAMState :: [TAMInst] -> TAMValue
initTAMState tamInsts = TAMValue {tsCode = tamInsts, tsCounter = 0, tsStack = []}

executeT :: TAMSt Stack
executeT = do tamInst <- nextInst
              execute tamInst
              if tamInst == HALT then stackT
                                 else executeT

-- fetch next TAM instruction from TAMSt
nextInst :: TAMSt TAMInst
nextInst = do tamSt <- stGetIO
              return (tsCode tamSt !! tsCounter tamSt)

stackT :: TAMSt Stack
stackT = do tsStack <$> stGetIO

popT :: TAMSt MTInt
popT = do tamSt <- stGetIO
          let stk = tsStack tamSt
          stUpdateIO (tamSt {tsStack = tail stk})
          return (head stk)

pushT :: MTInt -> TAMSt ()
pushT x = do tamSt <- stGetIO
             stUpdateIO (tamSt {tsStack = x : tsStack tamSt})


replaceT :: Int -> MTInt -> TAMSt ()
replaceT addr value = do tamSt <- stGetIO
                         let stk = tsStack tamSt
                         stUpdateIO (tamSt {tsStack = reverse (replaceByAddr addr value (reverse stk))})


replaceByAddr :: Int -> MTInt -> Stack  -> Stack
replaceByAddr addr value stk = left ++ (value : right)
                                where (left, _ : right) = splitAt addr stk

-- move the counter to the TAM instruction by given label
lCounter :: LName -> TAMSt ()
lCounter label = do tamSt <- stGetIO
                    stUpdateIO (tamSt {tsCounter = fromJust $ elemIndex (Label label) (tsCode tamSt)})

-- move the counter forward / add by 1
continueT :: TAMSt ()
continueT = do tamSt <- stGetIO
               stUpdateIO (tamSt {tsCounter = tsCounter tamSt + 1})


-- execute TAM instruction and update TAMstate
execute :: TAMInst -> TAMSt ()
-- end the main program
execute HALT = return ()
execute (LOADL value) = do pushT value
                           continueT
execute (LOAD addr) = do tamSt <- stGetIO
                         pushT (reverse (tsStack tamSt) !! addr)
                         continueT
execute (STORE addr) = do value <- popT
                          replaceT addr value
                          continueT
execute PUTINT = do value <- popT
                    lift (putStrLn ("output: " ++ show value))
                    continueT
execute GETINT = do lift (putStr "input: ")
                    lift (hFlush stdout)
                    input <- lift getLine
                    -- input validation check at runtime
                    case readMaybe input of
                        Nothing -> error "\nRuntime Error: Invalid input, only integer value can be accepted!"
                        Just value -> pushT value
                    continueT
execute (Label label) = continueT
execute (JUMPIFZ label) = do value <- popT
                             case value of
                                0 -> do lCounter label -- if 0 JUMP
                                        continueT
                                _ -> continueT         -- else, keep going
execute (JUMP label) = do lCounter label
                          continueT
-- Binary Operation                          
execute ADD = executeBiOp ADD
execute SUB = executeBiOp SUB
execute MUL = executeBiOp MUL
execute DIV = executeBiOp DIV
execute AND = executeBiOp AND
execute OR  = executeBiOp OR
execute LSS = executeBiOp LSS
execute GTR = executeBiOp GTR
execute EQL = executeBiOp EQL
-- Unary Operation
execute NOT = executeUnOp NOT
execute NEG = executeUnOp NEG

-- funtion used for execute Binary Operation                          
executeBiOp :: TAMInst -> TAMSt ()
executeBiOp tamInst = do v1 <- popT
                         v2 <- popT
                         pushT (evaluateBiOp tamInst v1 v2)
                         continueT

-- funtion used for execute Unary Operation                          
executeUnOp :: TAMInst -> TAMSt ()
executeUnOp tamInst = do v <- popT
                         pushT (evaluateUnOp tamInst v)
                         continueT

-- evaluation in haskell --------------------------------------------------------------------
-- Note: second 'BiOp' top
evaluateBiOp :: TAMInst -> MTInt -> MTInt -> MTInt
evaluateBiOp ADD x y = y + x
evaluateBiOp SUB x y = y - x
evaluateBiOp MUL x y = y * x
evaluateBiOp DIV x y = y `div` x
evaluateBiOp AND x y = y `intAND` x
evaluateBiOp OR x y = y `intOR` x
evaluateBiOp LSS x y = y `intLSS` x
evaluateBiOp GTR x y = y `intGTR` x
evaluateBiOp EQL x y = y `intEQL` x

evaluateUnOp :: TAMInst -> MTInt -> MTInt
evaluateUnOp NEG v = -v
evaluateUnOp NOT v = intNOT v




