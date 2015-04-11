.PHONY: all test clean dev

all: compile 

compile:
	ocamlbuild -quiet -r -I src -use-menhir main.byte

	# Compile java files and move to staticlib
	mkdir -p _build/staticlib
	cd src/java && javac GoLiteCloneable.java GoLiteList.java GoStructAbstract.java
	cp src/java/*.class _build/staticlib

	# Copy runtime support jasmin file
	cp src/jasmin/runtimesupport.j _build/staticlib

test:
	ocamlbuild -quiet -r -I src -I tests -pkg oUnit -use-menhir -menhir "menhir --strict --explain" lexer_test.native
	./lexer_test.native

dev:
	ocamlbuild  -r -I src -I dev -use-menhir -menhir "menhir --strict --explain" dev_tools.byte

clean:
	ocamlbuild -clean

