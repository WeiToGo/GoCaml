.PHONY: all test clean

all: compile 

compile:
	ocamlbuild -quiet -r -I src -use-menhir main.native

test:
	ocamlbuild -quiet -r -I src -I tests -pkg ounit -use-menhir lexer_test.native
	./lexer_test.native

clean:
	ocamlbuild -clean
