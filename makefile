HC=ghc
LD=ghc
CFLAGS=-c
LDFLAGS= 
.SUFFIXES: .hs .lhs .o

SRCS=$(wildcard.hs) $(wildcard *.lhs)
OBJECTFILES=Aufgabe5.o

all: $(OBJECTFILES)
	$(LD) $(LDFLAGS) $<

%.o: %.hs
	$(HC) $(CFLAGS) $<

clean:
	rm -vf *.o *.hi *.out
.PHONY: clean
