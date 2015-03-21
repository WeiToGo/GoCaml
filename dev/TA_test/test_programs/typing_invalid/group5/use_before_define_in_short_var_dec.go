package main

func main() {
	// In go this fails; we need to typecheck the expressions
	// before adding the new variables to the symbol table
	a, b, c := 1, a+1, b+1
}
