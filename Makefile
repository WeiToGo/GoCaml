.PHONY: all test clean

all: compile 

compile:
	ocamlbuild -quiet -r -I src -use-menhir main.native

test:
	ocamlbuild -quiet -r -I src -I tests -pkg oUnit -use-menhir -menhir "menhir --explain" lexer_test.native
	./lexer_test.native

clean:
	ocamlbuild -clean

