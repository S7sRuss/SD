src = StateMaybeMonad.hs Main.hs Parser.hs Lexer.hs Syntax.hs TypeSystem.hs CoreCompiler.hs Generator.hs 

all: $(src) runtime.s.bc
	ghc -w --make Main.hs -o core 

Parser.hs: Parser.y
	happy --ghc $<

Lexer.hs: Lexer.x
	alex --ghc $<

runtime.s:
	llvm-gcc -c -S -emit-llvm runtime.c

runtime.s.bc: runtime.s
	llvm-as runtime.s

.PHONY: clean

clean:
	rm -rf core *.o *.hi Lexer.hs Parser.hs runtime.s.bc runtime.s
