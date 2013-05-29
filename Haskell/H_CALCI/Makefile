src = Main.hs Parser.hs Lexer.hs Syntax.hs Semantics.hs

all: $(src)
	ghc -w --make Main.hs -o main 

Parser.hs: Parser.y
	happy --ghc $<

Lexer.hs: Lexer.x
	alex --ghc $<

.PHONY: clean

clean:
	rm -rf main *.o *.hi Lexer.hs Parser.hs
