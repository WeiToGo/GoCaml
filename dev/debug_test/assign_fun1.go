package main

func id(x int) int {
	return x
}

func incr(x int) int {
	return x+1
}

func main() {
	f := id
	var a int = f(3)
	f = incr
	var b int = f(a)
	if a < b {
		println("a < b")
	} else {
		println("a < b")
	}
}
