package main

type t int

func an_int() int {
	var x t
	// Return expression is not an int
	return x
}
