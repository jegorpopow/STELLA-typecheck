# Stella programming language typechecker

ITMO PLSTS course supplemetary

Lexer & Parser are generated via BNFC tool. 
Type checker implementation is placed into `TypeChecker.hs` file

## Supported features

* Core language
* Unit type
* Pairs & tuples 
* Records
* `let`-bindings
* Type ascriptions
* Sum types
* Lists
* Variants
* Fixpoint combinator

Additionally, tool supports following extensions

* Natural literals
* Nullary & multiparameter functions
* Nullary variant labels
* Structural patterns: WIP
* Nested fucntions: WIP
* `letrec`-bindings: WIP

## Build & run

The requirements are:
* GHC >= 8.10.7
* BNFC >= 2.9.6
* alex >= 3.3.0
* happy >= 1.20.1

Building command:

```bash
alex --ghc Lex.x 
happy --array --info --ghc --coerce Par.y 
ghc --make -hidir build/ -odir build/ *.hs
```

Running command: 
```bash
# Either
cat source.stella | ./Check
# or
./Check sourse-01.stella source-02.stella
```

# Tests

Folder `Oleg_testuite` contains [test suite creted by Oleg Haykin](https://github.com/ThatDraenGuy/stella_test_suite/)

Folder `Examples` contains my own examples (`pass` are well-typed ones and `fail` are ill-typed ones, basicly, name of test correspondes expected error code). 
