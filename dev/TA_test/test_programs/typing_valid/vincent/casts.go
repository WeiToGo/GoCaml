package main

func main() {
	type t int
	var x int = 5
	var y t = t(x)
	println(y)
}
