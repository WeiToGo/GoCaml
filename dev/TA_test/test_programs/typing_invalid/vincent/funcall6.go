package main

type t int

func f() t {
	var x t
	return x
}

func main() {
	// f() is a t, not an int
	var x int = f()
}
