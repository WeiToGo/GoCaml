package main

type t int
var x int

// x is not a type
func f(a int, b t, c x) {
}
