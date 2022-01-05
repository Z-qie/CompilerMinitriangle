{-
Program parser by Ziqi Yang, 2022

This file defines an Abstract Syntax Tree (AST) and 
functions that parse the given MT program as String 
into Abstract Syntax Tree (AST) data structure.

Grammar for Minitriangle program

program ::= let declarations in command
declaration = var identifier | var identifier := exp
declarations = declaration | declaration ; declaration
command ::= identifier := exp
            | if exp then command else command
            | while exp do command
            | getint (identifier)
            | printint (exp)
            | begin commands end
commands = command | command ; commands
-}

module ASTParser where

import Control.Applicative
import FunParser

-- AST data structure ----------------------------------------------------------------
data AST
  = LitInteger Int
  | Program Declarations Command  -- the root AST must start from a Program
  | Var Identifier
  | BinOp BinOperator AST AST
  | UnOp UnOperator AST
  | Conditional AST AST AST
  deriving (Eq, Show)

type Identifier = String
type Commands = [Command]
type Declarations = [Declaration]

-- AST subtree for Binary Operator
data BinOperator
  = Addition        -- (+)
  | Subtraction     -- (-)
  | Multiplication  -- (*)
  | Division        -- (/)
  | Conjunction     -- (&&)
  | Disjunction     -- (||)
  | LssOp           -- (<)
  | LeqOp           -- (<=)
  | GtrOp           -- (>)
  | GeqOp           -- (>=)
  | EqOp            -- (==)
  | NeqOp           -- (!=)
  deriving (Eq, Show, Enum)

-- AST subtree for Unary Operator
data UnOperator = Negation | NegBool
  deriving (Eq, Show)

-- AST subtree for command of the program
data Command
  = Assignment Identifier AST
  | IfThenElse AST Command Command
  | WhileDo AST Command
  | GetInt Identifier
  | PrintInt AST
  | BeginEnd Commands
  deriving (Eq, Show)

-- AST subtree for declaration of the program
data Declaration = Decl Identifier | Init Identifier AST
  deriving (Eq, Show)

-- parser for Program --------------------------------------------------------------------
mtProg :: Parser AST
mtProg = do symbol "let"
            ds <- decls
            symbol "in"
            c <- cmd
            do i <- item
               error ("\nParsing Error: Invalid Syntax: \'" ++ show i ++ "\': must be  \"let Declarations in Command\"!")
               <|>
               return (Program ds c)
     <|> error "\nParsing Error: Invalid MT Program: must be  \"let Declarations in Command\"!"

-- Declarations --------------------------------------------------------------------
decls :: Parser Declarations
decls = do d <- decl
           ds <- many (do symbol ";"
                          decl)
           return (d:ds)

decl :: Parser Declaration
decl = do symbol "var"
          do varName <- checkKeyword
             do symbol ":="
                Init varName <$> expr
                <|>
                return (Decl varName)
             <|> -- if varName is any keyword
             do word <- items
                error ("\nParsing Error: Invalid Variable Name at word: \"" ++ word ++ "\": must not be any reserved keyword!")
       <|> -- if "var" not read
       error "\nParsing Error: Invalid MT declaration syntax: must be \"var varName\" or \"var varName := MTInt\" in the form of \"Declaration ; Declarations\"!"

-- Commands --------------------------------------------------------------------
cmd :: Parser Command
cmd = do beginEnd
  <|> do whileDo
  <|> do ifThenElse
  <|> do getInt
  <|> do printInt
  <|> do assign
  <|> (do word <- items
          error ("\nParsing Error: Invalid MT Command Format at word: \"" ++ word ++ "\"!"))

cmds :: Parser Commands
cmds = do c <- cmd
          cs <- many (do symbol ";"
                         cmd)
          return (c:cs)
   <|> (do word <- items
           error ("\nParsing Error: Invalid MT Commands Format at word: \"" ++ word ++ "\"!"))

beginEnd :: Parser Command
beginEnd = do symbol "begin"
              cs <- cmds
              symbol "end"
              return (BeginEnd cs)

whileDo :: Parser Command
whileDo = do symbol "while"
             e <- expr
             symbol "do"
             WhileDo e <$> cmd

ifThenElse :: Parser Command
ifThenElse = do symbol "if"
                e <- expr
                symbol "then"
                c1 <- cmd
                symbol "else"
                IfThenElse e c1 <$> cmd

getInt :: Parser Command
getInt = do symbol "getint"
            i <- parens identifier
            return (GetInt i)

printInt :: Parser Command
printInt =  do symbol "printint"
               e <- parens expr
               return (PrintInt e)

assign :: Parser Command
assign = do i <- identifier
            symbol ":="
            Assignment i <$> expr

-- Expressions --------------------------------------------------------------------
{-
Grammar for expressions (either arith or bool):
We allow arith expressions to occur as bool expressions in parentheses
Conditionals must be in parentheses, except at the top level

  exp ::= bexp | bexp ? bexp : bexp

  bexp ::= cexp | cexp || bexp
  cexp ::= bterm | bterm && cexp
  bterm ::= aexp | aexp `op` aexp
             where `op` is one of <,<=,>,>=,==,!=

  aexp ::= mexp | mexp + aexp | mexp - aexp
  mexp ::= aterm | aterm * mexp | aterm / mexp
  aterm ::= intLit | - aterm | ! aterm | ( exp )
-}

expr :: Parser AST
expr = do b <- bexp
          do
            symbol "?"
            e0 <- bexp
            symbol ":"
            e1 <- bexp
            return (Conditional b e0 e1)
            <|> return b
        <|> (do word <- items
                error ("\nParsing Error: Invalid Expression Syntax at word: \"" ++ word ++ "\"!"))

bexp :: Parser AST
bexp = do
  e0 <- cexp
  do
    symbol "||"
    e1 <- bexp
    return (BinOp Disjunction e0 e1)
    <|> return e0

cexp :: Parser AST
cexp = do
  e0 <- bterm
  do
    symbol "&&"
    e1 <- cexp
    return (BinOp Conjunction e0 e1)
    <|> return e0

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop =
  choice
    [ symbol "<=" >> return LeqOp,
      symbol "<" >> return LssOp,
      symbol ">=" >> return GeqOp,
      symbol ">" >> return GtrOp,
      symbol "==" >> return EqOp,
      symbol "!=" >> return NeqOp
    ]

bterm :: Parser AST
bterm = do
  e0 <- aexp
  do
    op <- relop
    e1 <- aexp
    return (BinOp op e0 e1)
    <|> return e0

addminus :: Parser BinOperator
addminus =
  choice
    [ symbol "+" >> return Addition,
      symbol "-" >> return Subtraction
    ]

-- For left-associativity, we use an auxiliary function aexp'
--    that keeps a functional accumulator

aexp :: Parser AST
aexp = aexp' id

aexp' :: (AST -> AST) -> Parser AST
aexp' f = do
  e0 <- mexp
  do
    op <- addminus
    aexp' (BinOp op (f e0))
    <|> return (f e0)

multdiv :: Parser BinOperator
multdiv =
  choice
    [ symbol "*" >> return Multiplication,
      symbol "/" >> return Division
    ]

mexp :: Parser AST
mexp = mexp' id

mexp' :: (AST -> AST) -> Parser AST
mexp' f = do
  e0 <- aterm
  do
    op <- multdiv
    mexp' (BinOp op (f e0))
    <|> return (f e0)

aterm :: Parser AST
aterm = (natural >>= return . LitInteger)
            <|> (do symbol "-"
                    e <- aterm
                    return (UnOp Negation e))
            <|> (do symbol "!"
                    b <- aterm
                    return (UnOp NegBool b))
            <|> parens expr
            <|> (do i <- identifier
                    return (Var i))
