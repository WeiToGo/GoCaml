package main

func main() {
	b, c := 2, 3
	println(b, c)

	// Already-declared var on lhs of shortdecl has type mismatch
	b, d := "hello", 5
	println(b, d)
}
