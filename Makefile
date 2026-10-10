build:
	ghc -o my_parser Main.hs Lexer.hs

run: build 
	./my_parser

