# Stella programming language typechecker

ITMO PLSTS course supplementary

Lexer & Parser are generated via BNFC tool. 
Type checker implementation is placed into `TypeChecker.hs` file

## Supported features

`extend with` section is not checked, the tool considers all the following extensions are
on all the time (the ony exception is `#type-reconstruction`, which significantly
influences the type-checking process):

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
* Church-style System F 
* Type reconstructions (for limited suubset of programs), including

Additionally, the tool supports following extensions:

* Natural literals
* Nullary & multiparameter functions
* Nullary variant labels
* Nested funсtions
* `letrec`-bindings
* Structural patterns

## Build & run

The requirements are:
* GHC >= 8.10.7
* alex >= 3.3.0
* happy >= 1.20.1
* mtl >= 2.3.1 **(New!)** 

Makefile is provided

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

Folder `Oleg_testuite` contains [test suite created by Oleg Haykin](https://github.com/ThatDraenGuy/stella_test_suite/)

Folder `Examples` contains my own examples (`pass` are well-typed ones and `fail` are ill-typed ones. Basicly, name of the test corresponds expected error code). 
