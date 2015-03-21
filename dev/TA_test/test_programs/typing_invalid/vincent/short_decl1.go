package main

func main() {
	b, c := 2, 3
	println(b, c)

	// No new decl on the lhs of a short decl
	b, c := 4, 5
	println(b, c)
}
