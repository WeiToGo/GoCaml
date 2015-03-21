package main

type t int

func main() {
	var a t
	// Cannot add t and int
	var b int = a + 1
}
