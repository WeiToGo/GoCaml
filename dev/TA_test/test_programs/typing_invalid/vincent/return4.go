package main

type t int

func a_t() t {
	var x int
	// Return expression is not a t
	return x
}
