package main

func f(x int) {
}

func main() {
	type t int
	var x t
	// Argument is not an int
	f(x)
}
