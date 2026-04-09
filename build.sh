#!/bin/bash

alex --ghc Lex.x 
happy --array --info --ghc --coerce Par.y 
ghc --make -hidir build/ -odir build/ *.hs
