ALEX   = alex
HAPPY  = happy
GHC    = ghc

ALEX_FLAGS   = --ghc
HAPPY_FLAGS  = --array --info --ghc --coerce
GHC_FLAGS    = -hidir build/ -odir build/

EXEC = Check

all: $(EXEC)

build/:
	mkdir -p build/

Lex.hs: Lex.x | build/
	$(ALEX) $(ALEX_FLAGS) $<

Par.hs: Par.y | build/
	$(HAPPY) $(HAPPY_FLAGS) $<

HS_FILES = $(wildcard *.hs) Lex.hs Par.hs

$(EXEC): $(HS_FILES) | build/
	$(GHC) --make $(GHC_FLAGS) *.hs -o $(EXEC)

clean:
	rm -f Lex.hs Par.hs $(EXEC)
	rm -rf build/

.PHONY: all clean