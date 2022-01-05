# Compiler Coursework

### Build a compiler for MiniTriangle language

Open terminal under the directory containing Haskel files, 

and Run

``` ghc Main -o mtc ``` or ``` ghc Main.hs -o mtc ```

On success, an executable named mtc will be generated with .hi and .o for each corresponding Haskell file,

then you can pass .tam and .mt files into the compiler like so

``` 
    ./mtc program.tam
    ./mtc program.mt 
```

---

### Error Checking:

#### Parsing Time Error

1. Parsing Error: Invalid Syntax/MT Program: [at "word"] must be "let Declarations in Command!"
   - when any invalid MT code at the "let in" keyword or at the end of the program
2. Parsing Error: Invalid Variable Name at word: "word" : must not be any reserved keyword!"
   - when any variable name is a reserved keyword
3. Parsing Error: Variables must start with a lower or upper case letter!
   - when any declared variable name start with a char that is not alphanumeric
4. Parsing Error: Invalid MT declaration syntax: must be "var varName" or "var varName := MTInt" in the form of "Declaration ; Declarations!"
   - when any variable name declaration contains a syntax error or wrong placement of semicolon
5. Parsing Error: Invalid MT Command Format at word: "word" 
   - when any syntax error exist in any command
6. Parsing Error: Invalid Expression Syntax at word: "word" 
   - when any error exist in any expression

#### Runtime Error

1. Runtime Error: Invalid input, only integer value can be accepted!

#### Compile Time Error (when generate TAM code)

1. Compiling Error: Redefined Variable: Variable "var" has already been defined!
   - when redefining a variable
2. Compiling Error: Variable "var" does not exist!
   - when accessing a variable that has not been defined
3. Compiling Error: Undefined TAMInst
   - when trying compile an AST operation with no corresponding TAM instruction. (for further development debug)

---

### Grammar for Minitriangle program

program ::= let declarations in command

declaration = var identifier | var identifier := exp

declarations = declaration | declaration ; declaration

command ::= identifier := exp

​            | if exp then command else command

​            | while exp do command

​            | getint (identifier)

​            | printint (exp)

​            | begin commands end

commands = command | command ; commands

---

### Grammar for expressions (either arith or bool):

We allow arith expressions to occur as bool expressions in parentheses

Conditionals must be in parentheses, except at the top level

  exp ::= bexp | bexp ? bexp : bexp

  bexp ::= cexp | cexp || bexp

  cexp ::= bterm | bterm && cexp

  bterm ::= aexp | aexp `op` aexp

​             where `op` is one of <,<=,>,>=,==,!=

  aexp ::= mexp | mexp + aexp | mexp - aexp

  mexp ::= aterm | aterm * mexp | aterm / mexp

  aterm ::= intLit | - aterm | ! aterm | ( exp )

---

### MT program reseved words:

- let
- in
- var
- if
- then
- else
- while
- do
- getint
- begin
- end

