.PHONY: all test clean dev

all: compile 

compile:
	ocamlbuild -quiet -r -I src -use-menhir main.byte

test:
	ocamlbuild -quiet -r -I src -I tests -pkg oUnit -use-menhir -menhir "menhir --explain" lexer_test.native
	./lexer_test.native

dev:
	ocamlbuild  -r -I src -use-menhir -menhir "menhir --explain" dev_tools.byte

clean:
	ocamlbuild -clean

